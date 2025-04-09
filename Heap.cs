using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Text;

namespace inet;

public ref struct Heap
{
    const ulong FREE = ulong.MaxValue & ~0b111UL;

    public Span<ulong> Cells;
    public unsafe ulong* LastFree;
    public int Head;

    public Heap(Span<ulong> cells)
    {
        unsafe
        {
            Cells = cells;
            LastFree = default;
            Head = 0;
        }
    }

    private unsafe ulong GetIndex(ulong* x)
    {
        var root = (ulong*)Unsafe.AsPointer(ref Cells[0]);
        Debug.Assert(x >= root && x < &root[Cells.Length]);
        return ((ulong)x - (ulong)root) / sizeof(ulong);
    }

    private unsafe Span<ulong> RawAlloc()
    {
        Span<ulong> pair;
        if (LastFree != null)
        {
            pair = new Span<ulong>(LastFree, 2);
            var freeNode = pair[0];
            LastFree =
                freeNode == FREE ? null : (ulong*)freeNode;
        }
        else
        {
            if (Head + 2 > Cells.Length)
                throw new OutOfMemoryException("Ran out of heap space");

            pair = Cells.Slice(Head, 2);
            Head += 2;
        }

        return pair;
    }

    public Port AllocNode(PortKind k, ushort combId)
    {
        var pair = RawAlloc();
        pair[0] = 0;
        pair[1] = 0;
        ulong addr;
        unsafe
        {
            addr = (ulong)Unsafe.AsPointer(ref pair[0]);
        }
        return new Port(k, combId, addr);
    }

    public override string ToString()
    {
        unsafe
        {
            var baseAddr = (ulong*)Unsafe.AsPointer(ref Cells[0]);

            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < Head; i++)
            {
                var p = new Port(Cells[i]);
                sb.Append($"{(ulong)&baseAddr[i]:X08}:  {p, 16}");

                if (!p.IsFreeNode)
                {
                    sb.Append($"   x{GetIndex(&baseAddr[i])}");
                    if (p.Kind is PortKind.Comb or PortKind.Wire)
                    {
                        sb.Append($" = {Pretty(p)}");
                    }
                }

                sb.AppendLine();
            }

            return sb.ToString();
        }
    }

    public string Pretty(Port p, HashSet<Wire>? seen = null)
    {
        seen ??= [];
        string[] names = p.Kind == PortKind.Comb ? ["fn", "tup", "dup"] : ["add", "seq", "print"];
        switch (p.Kind)
        {
            case PortKind.ExtFn or PortKind.Operator or PortKind.Comb:
                var (s, ( lbl, f)) = p.Kind switch
                {
                    PortKind.Comb => ("", (p.Label, false)),
                    PortKind.Operator =>
                        p.Operator switch
                        {
                            OperatorKind.Branch => ("?", (p.Label, false)),
                            OperatorKind.Lift => ("LIFT", (p.Label, false)),
                            OperatorKind.Lower => ("LOWER", (p.Label, false)),
                        },
                    _ => ("EFN:", p.GetExtFn()),
                };
                var label = $"dup{lbl-2}";
                var flip = f ? "^" : "";
                if (lbl < names.Length)
                    label = names[lbl];
                return $"{s}{flip}{label}({Pretty(p.Aux.Left.ToPort(), seen)} {Pretty(p.Aux.Right.ToPort(), seen)})";
            case PortKind.Wire:
                if (seen.Add(p.Wire))
                {
                    while (p.Kind == PortKind.Wire)
                    {
                        var x = p.Wire.Load();
                        if (!x.IsAssigned)
                            break;
                        p = x;
                    }
                }

                if (p.Kind != PortKind.Wire)
                    return Pretty(p, seen);

                seen.Add(p.Wire);
                unsafe
                {
                    return $"x{GetIndex(p.Wire.Raw)}";
                }

            case PortKind.Unassigned:
            case PortKind.ExtVal:
            case PortKind.Global:
            case PortKind.Eraser:
            default:
                return p.ToString();
        }
    }

    public unsafe void Free(ulong* addr)
    {
        Debug.Assert(addr >= Unsafe.AsPointer(ref Cells[0]));
        Debug.Assert(addr < Unsafe.AsPointer(ref Cells[Head]));

        *addr = FREE;

        var freeHead = (ulong*)(((ulong)addr) & ~0b1111UL);
        if (new Port(*freeHead).IsFreeNode
            && freeHead[0] == FREE
            && freeHead[1] == FREE)
        {
            if (LastFree != null)
                *freeHead = (ulong)LastFree;
            LastFree = freeHead;
        }
    }
}


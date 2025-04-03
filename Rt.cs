using System.Diagnostics;
using System.Text;

namespace  inet;

public static class Builtins
{
    public const ushort AddLabel = 0;

    public static void RegisterBuiltins(ref Rt rt)
    {
        rt.ExtFns.Add(Add);
    }
    
    static ulong Add(ulong a, ulong b) => a + b;
}

public ref struct Rt
{
    public Heap Heap;
    public readonly Stack<(Port, Port)> ActiveFast = new();
    public readonly Stack<(Port, Port)> ActiveSlow = new();
    public readonly List<Program> Globals = new();
    public readonly List<Func<ulong, ulong, ulong>> ExtFns = new();

    readonly List<Port> _registers = new();

    public Rt(Heap h)
    {
        Heap = h;
        Builtins.RegisterBuiltins(ref this);
    }

    private void FreeWire(Wire w)
    {
        unsafe
        {
            Heap.Free(w.Raw);
        }
    }

    private Port FollowWire(Port p)
    {
        while (p.Kind == PortKind.Wire)
        {
            var x = p.Wire.Load();
            if (!x.IsAssigned)
                break;
            FreeWire(p.Wire);
            p = x;
        }

        return p;
    }

    public void LinkWire(Wire w, Port p)
    {
        p = FollowWire(p);
        var oldX = w.Swap(p);
        if (oldX.IsAssigned)
        {
            FreeWire(w);
            Link(oldX, p);
        }
    }

    public void Link(Port a, Port b)
    {
        if (a.Kind == PortKind.Wire)
            LinkWire(a.Wire, b);
        else if (b.Kind == PortKind.Wire)
            LinkWire(b.Wire, a);
        else if ((a.Kind is PortKind.Eraser or PortKind.Global && b.Kind is PortKind.Eraser or PortKind.Global)
            || (a.Kind is PortKind.Eraser or PortKind.ExtVal && b.Kind is PortKind.Eraser or PortKind.ExtVal))
        {
            // Erase
        }
        else
        {
            // Annihilate
            if ((a.Kind == b.Kind && a.Kind is PortKind.Comb or PortKind.ExtFn && a.Label == b.Label) 
                // Copy 
                || (a.Kind is PortKind.Eraser or PortKind.ExtVal)
                || (b.Kind is PortKind.Eraser or PortKind.ExtVal)) 
                ActiveFast.Push((a, b));
            else
                ActiveSlow.Push((a, b));
        }
    }

    public void Interact(Port a, Port b, bool sym = true)
    {
        if ((a.Kind == PortKind.Global && b.Kind == PortKind.Comb)
            || (a.Kind == PortKind.Eraser)
            || (a.Kind == PortKind.ExtVal && b.Kind == PortKind.Comb))
        {
            // Copy
            LinkWire(b.Aux.Left, a);
            LinkWire(b.Aux.Right, a);
        } else if (a.Kind == PortKind.Global)
        {
            // Expand
            Debug.Assert(a.ExtVal < (ulong)Globals.Count);
            Execute(Globals[(int)a.ExtVal], b);
        } else if (a.Kind == b.Kind
                   && a.Kind is PortKind.Comb or PortKind.ExtFn
                   && a.Label == b.Label)
        {
            // Annihilate
            LinkWire(a.Aux.Left, b.Aux.Left.ToPort());
            LinkWire(a.Aux.Right, b.Aux.Right.ToPort());
        } else if (a.Kind is PortKind.Comb or PortKind.ExtFn
                   && b.Kind is PortKind.Comb or PortKind.ExtFn)
        {
            // Commute
            var al = Heap.AllocNode(PortKind.Comb, a.Label);
            var ar = Heap.AllocNode(PortKind.Comb, a.Label);
            var bl = Heap.AllocNode(PortKind.Comb, b.Label);
            var br = Heap.AllocNode(PortKind.Comb, b.Label);

            LinkWire(b.Aux.Right, al);
            LinkWire(b.Aux.Left, ar);
            LinkWire(a.Aux.Left, bl);
            LinkWire(a.Aux.Right, br);

            LinkWire(al.Aux.Left, bl.Aux.Right.ToPort());
            LinkWire(al.Aux.Right, br.Aux.Left.ToPort());
            LinkWire(ar.Aux.Left, bl.Aux.Left.ToPort());
            LinkWire(ar.Aux.Right, al.Aux.Right.ToPort());
        } else if (a.Kind == PortKind.ExtFn && b.Kind == PortKind.ExtVal)
        {
            // Call
            var argR = a.Aux.Left.Load();
            if (argR.IsAssigned)
            {
                if (argR.Kind == PortKind.ExtVal)
                {
                    FreeWire(a.Aux.Left);
                    var res = InvokeExtFn(a, b.ExtVal, argR.ExtVal);
                    LinkWire(a.Aux.Right, Port.FromExtVal(res));
                    return;
                }
            }
            
            var newFn = Heap.AllocNode(PortKind.ExtFn, a.SwappedLabel);
            LinkWire(a.Aux.Left, newFn);
            LinkWire(newFn.Aux.Left, b);
            LinkWire(newFn.Aux.Right, a.Aux.Right.ToPort());
        }
        else if (sym)
        {
            Interact(b, a, sym: false);
        }
        else
        {
            throw new InvalidOperationException($"Invalid interaction: {a} <> {b}");
        }
    }

    ulong InvokeExtFn(Port fn, ulong a, ulong b)
    {
        var (id, swp) = fn.GetExtFn();
        Debug.Assert(id < ExtFns.Count);
        
        if (swp)
            return ExtFns[id](b, a);
        
        return ExtFns[id](a, b);
    }

    void LinkRegister(int reg, Port p)
    {
        Debug.Assert(reg < _registers.Count);
        var x = _registers[reg];
        if (x.IsAssigned)
        {
            _registers[reg] = default;
            Link(p, x);
        }
        else
        {
            _registers[reg] = p;
        }
    }

    public void Execute(Program prog, Port p)
    {
        const int ROOT = 0;

        _registers.EnsureCapacity(prog.Registers);
        while (_registers.Count < prog.Registers || _registers.Count < 1)
        {
            _registers.Add(new Port());
        }

        LinkRegister(ROOT, p);

        foreach (var inst in prog.Instructions)
        {
            if (inst is BinaryInst
                {
                    Kind: var k, Label: var lbl,
                    Regs: (var a, var l, var r)
                })
            {
                Debug.Assert(k is PortKind.Comb or PortKind.ExtFn);
                var node = Heap.AllocNode(k, lbl);
                LinkRegister(a.Index, node);
                LinkRegister(l.Index, node.Aux.Left.ToPort());
                LinkRegister(r.Index, node.Aux.Right.ToPort());
            } else if (inst is NilaryInst { Value: var x, Reg: var q })
            {
                Debug.Assert(x.Kind is PortKind.Eraser or PortKind.Global or PortKind.ExtVal);
                LinkRegister(q.Index, x);
            }
        }

        // All register values must be used an even number of times
        Debug.Assert(_registers.All(x => !x.IsAssigned));
    }

    public override string ToString()
    {
        StringBuilder sb = new StringBuilder();
        sb.Append(Heap.ToString());
        foreach (var (x,y) in ActiveFast)
        {
            sb.AppendLine($"- {Heap.Pretty(x)} <> {Heap.Pretty(y)}");
        }
        return sb.ToString();
    }
}

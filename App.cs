using System.Diagnostics;
using Spectre.Console;

namespace inet;

interface IVisualizer
{
    void Visualize(Rt rt, List<string>? log = null);
}

class TUIVisualizer : IVisualizer
{
    public float[] WriteTimes;
    public (int Width, int Height) HeapView;

    public TUIVisualizer()
    {
        HeapView = (16, 24);
        WriteTimes = new float[HeapView.Width * HeapView.Height];
    }

    public void Visualize(Rt rt, List<string>? log = null)
    {
        AnsiConsole.Clear();
        VisualizeHeap(rt.Heap);
        AnsiConsole.WriteLine();
        AnsiConsole.ResetColors();
        AnsiConsole.Write(new Rule());
        VisualizeActivations(rt.ActiveFast);
        AnsiConsole.WriteLine();
        VisualizeActivations(rt.ActiveSlow);
        if (log is not null)
        {
            AnsiConsole.WriteLine();
            AnsiConsole.Write(new Rule());
            AnsiConsole.Foreground = Color.Grey89;
            foreach (var msg in log)
            {
                Console.WriteLine(msg);
            }
        }
    }

    public void VisualizeHeap(Heap h)
    {
        for (int row = 0; row < HeapView.Height; ++row)
        {
            if (row * HeapView.Width >= h.Head)
                break;

            for (int col = 0; col < HeapView.Width; ++col)
            {
                var idx = row * HeapView.Width + col;
                if (idx >= h.Head)
                    break;

                DrawPortCell(new Port(h.Cells[idx]), WriteTimes[idx]);
            }
            AnsiConsole.WriteLine();
        }
    }

    public void VisualizeActivations(Stack<(Port, Port)> activations)
    {
        var acts = activations.ToArray();
        foreach((var a, _) in acts.Reverse())
            DrawPortCell(a, 0f);
        AnsiConsole.WriteLine();
        foreach((_, var b) in acts.Reverse())
            DrawPortCell(b, 0f);
    }

    void DrawPortCell(Port port, float t)
    {
        unsafe
        {
            Color background = Color.Black;
            Color writeColor = Color.Grey70;

            background = background.Blend(writeColor, float.Clamp(t, 0f, 1f));
            Color foreground = port.Kind switch
            {
                PortKind.Wire => Color.SteelBlue,
                PortKind.ExtVal => Color.Orange3,
                PortKind.ExtFn => Color.BlueViolet,
                PortKind.Global => Color.Cyan3,
                PortKind.Comb => Color.Green,
                PortKind.Operator => Color.Pink3,
                PortKind.Eraser => Color.Red,
                PortKind.Unassigned when port.IsFreeNode => Color.Grey11,
                PortKind.Unassigned => Color.Grey23,
            };

            var text = port.Kind switch
            {
                PortKind.Wire => $"{(((ulong)port.Wire.Raw) >> 3)&0xFF:X02}",
                PortKind.ExtVal => $"{(port.ExtVal.Raw >> 4)&0xFF:X02}",
                PortKind.ExtFn => $"{port.Label&0xFF:X02}",
                PortKind.Global => $"{port.Addr&0xFF:X02}",
                PortKind.Comb => $"{port.Label:X02}" ,
                PortKind.Operator =>
                    port.Operator switch
                    {
                        OperatorKind.Branch => "??",
                        OperatorKind.Lift => "^^",
                        OperatorKind.Lower => ",,"
                    },
                PortKind.Eraser => "!!",
                PortKind.Unassigned when port.IsFreeNode =>
                    port.RawValue != (ulong.MaxValue & ~0b111UL) ? "!." : "..",
                PortKind.Unassigned => "--",
                _ => "%%",
            };

            AnsiConsole.Background = background;
            AnsiConsole.Foreground = foreground;
            AnsiConsole.Write(text);
        }
    }
}

class TextVisualizer : IVisualizer
{
    public void Visualize(Rt rt, List<string>? log = null)
    {
        Console.WriteLine(rt.ToString());
        if (log is not null)
            foreach (var msg in log)
            {
                Console.WriteLine($"- {msg}");
            }
    }
}

class Application
{
    enum C
    {
        Fn,
        Dup,
    }

    static void Main(string[] args)
    {
        try
        {
            Run();
        }
        catch (Exception e)
        {
            AnsiConsole.WriteLine();
            AnsiConsole.WriteException(e);
            Console.ReadKey();
        }
    }

    public static void Run()
    {
        var baseHeap = new ulong[256];
        var heap = new Heap(baseHeap);
        var rt = new Rt(heap);
        var log = new List<string>();
        var moneyTy = rt.NewUniqTy<ulong>("money");

        rt.Globals.AddRange(Compiler.Compile(new Module([
            new Def("main", Expr.Lam(["io"],
                    Expr.Let("xs", Expr.ExtCall(4, Expr.I32(3), Expr.I32(9)),
                        Expr.Print(Expr.Var("io"), Expr.ExtCall(5, Expr.Var("xs"), Expr.I32(0)))))),
            new Def("id", Expr.Lam(["x"], Expr.Var("x"))),
            new Def("pair", Expr.Lam(["x"], Expr.Tup(Expr.Var("x"), Expr.Var("x")))),
        ])));
        rt.ExtFns.Add((ref Rt rt, ExtVal io, ExtVal val) =>
        {
            Debug.Assert(io.Type == Rt.IOTy);
            ref var ioVal = ref io.Ref<ulong>();
            ioVal.Value++;
            log.Add($"PRINT[{io}]: {val}");
            val.Drop(ref rt);
            return io;
        });
        rt.ExtFns.Add((ref Rt rt, ExtVal amount, ExtVal unused) =>
        {
            unused.Drop(ref rt);
            Debug.Assert(amount.Type == Rt.I32Ty);
            return ExtVal.MakeUniq(moneyTy, amount.Imm);
        });
        rt.ExtFns.Add((ref Rt rt, ExtVal length, ExtVal initVal) =>
        {
            Debug.Assert(length.Type == Rt.I32Ty);
            ref var arr = ref Slots.Alloc((uint)length.Imm);
            var elms = arr.AsSpan();
            for (int i = 0; i < elms.Length - 1; i++)
            {
                elms[i] = initVal.Dup(ref rt);
            }

            if (elms.Length > 0)
            {
                elms[^1] = initVal;
            }
            else
            {
                initVal.Drop(ref rt);
            }

            return ExtVal.MakeArray(ref arr);
        });
        rt.ExtFns.Add((ref Rt rt, ExtVal arr, ExtVal i) =>
        {
            Debug.Assert(arr.Type == Rt.ArrayTy);
            Debug.Assert(i.Type == Rt.I32Ty);
            var vals = arr.Array().AsSpan();
            Debug.Assert(i.Imm < (ulong)vals.Length);
            var r = vals[(int)i.Imm].Dup(ref rt);
            arr.Drop(ref rt);
            return r;
        });

        rt.InFastPhase = false;
        rt.Interact(Port.Global(0), Port.FromExtVal(ExtVal.MakeRef(Rt.IOTy, 0UL)));
        Console.WriteLine(rt.ToString());

        var v = new TUIVisualizer();
        v.Visualize(rt, log);
        Console.ReadKey();

        while (true)
        {
            rt.InFastPhase = true;
            while (rt.ActiveFast.Count > 0)
            {
                var (a, b) = rt.ActiveFast.Pop();
                rt.Interact(a, b);
                v.Visualize(rt, log);
                Console.ReadKey();
            }

            if (rt.ActiveSlow.Count > 0)
            {
                rt.InFastPhase = false;
                var (a, b) = rt.ActiveSlow.Pop();
                rt.Interact(a, b);
                v.Visualize(rt, log);
                Console.ReadKey();
            }
            else
            {
                break;
            }
        }
    }
}

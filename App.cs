using System.Diagnostics;
using Spectre.Console;

namespace inet;

class Visualizer
{
    public float[] WriteTimes;
    public (int Width, int Height) HeapView;

    public Visualizer()
    {
        HeapView = (32, 24);
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
                PortKind.Branch => Color.Pink3,
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
                PortKind.Branch => $"??",
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
        Cell<ulong> ioCell = new(0);

        // var prog = Program.Build((bin, nil) =>
        // {
        //     /*
        //     // fn(_ r2)
        //     bin(PortKind.Comb, (ushort)C.Fn, 0, 1, 2);
        //     nil(Port.Eraser(), 1);
        //     // fn(id r2) = fn(dup(x0 x1) r)
        //     bin(PortKind.Comb,  (ushort)C.Fn, 3, 4, 2);
        //     bin(PortKind.Comb,  (ushort)C.Fn, 3, 5, 6);
        //     bin(PortKind.Comb, (ushort)C.Dup, 5, 7, 8);
        //     // id = fn(x x)
        //     bin(PortKind.Comb, (ushort)C.Fn, 4, 9, 9);
        //     // x1 = fn(x0 r)
        //     bin(PortKind.Comb, (ushort)C.Fn, 8, 7, 6);
        //     */
        //
        //     // fn(PRINT(99 r) r)
        //     bin(PortKind.Comb, (ushort)C.Fn, 0, 1, 2);
        //     bin(PortKind.ExtFn,           0, 1, 3, 2);
        //     nil(Port.FromExtVal(99), 3);
        // });

        var prog = Compiler.CompileExpr(
            Expr.Do(
                Expr.Print(Expr.Var("io"), Expr.Call(Expr.Lam(["x"], Expr.Var("x")), Expr.I32(1))),
                Expr.Print(Expr.Var("io"), Expr.Call(Expr.Lam(["x", "y"], Expr.Add(Expr.Var("x"), Expr.Var("x"))), Expr.I32(9), Expr.I32(2))),
                Expr.Print(Expr.Var("io"), Expr.Call(Expr.Lam([], Expr.I32(3))))
            ));

        Console.WriteLine(prog);

        rt.Globals.Add(prog);
        rt.ExtFns.Add((io, val) =>
        {
            ref var ioVal = ref io.Ref<ulong>();
            ioVal.Value++;
            log.Add($"PRINT[{io}]: {val}");
            val.Drop();
            return io;
        });


        rt.Interact(Port.Global(0), Port.FromExtVal(ExtVal.FromRef(ref ioCell)));
        Console.WriteLine(rt.ToString());

        /*
           main:
             fn(_ rr)
             fn(id rr) = fn(dup(x0 x1) r)
             id = fn(x x)
             x1 = fn(x0 r)
         */

        /*
        var id = rt.Heap.AllocNode(PortKind.Comb, (ushort)C.Fn);
        var app = rt.Heap.AllocNode(PortKind.Comb, (ushort)C.Fn);
        var f = rt.Heap.AllocNode(PortKind.Comb, (ushort)C.Fn);
        var dup = rt.Heap.AllocNode(PortKind.Comb, (ushort)C.Dup);
        var fApp = rt.Heap.AllocNode(PortKind.Comb, (ushort)C.Fn);
        // id = fn(x x)
        rt.LinkWire(id.Aux.Left, id.Aux.Right.ToPort());
        // f = fn(dup(x0 x1) r)
        rt.LinkWire(f.Aux.Left, dup);
        // x1 = fn(x0 r)
        rt.LinkWire(dup.Aux.Right, fApp);
        rt.LinkWire(fApp.Aux.Left, dup.Aux.Left.ToPort());
        rt.LinkWire(fApp.Aux.Right, f.Aux.Right.ToPort());
        // fn(EXT id) = f
        rt.LinkWire(app.Aux.Left, Port.FromExtVal());
        rt.LinkWire(app.Aux.Right, id);
        rt.Link(app, f);
        */

        var v = new Visualizer();
        v.Visualize(rt, log);
        Console.ReadKey();

        // Console.WriteLine(rt.ToString());
        //
        // Console.WriteLine($"-------------------");
        //
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
        Debug.Assert(ioCell.RefCount == 0);
    }
}

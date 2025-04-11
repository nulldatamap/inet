namespace inet;

public record Register(int Index)
{
    public override string ToString() => $"r{Index}";
}

public abstract class Inst
{
}
public sealed class BinaryInst(PortKind k, ushort lbl, Register a, Register l, Register r) : Inst
{
    public readonly PortKind Kind = k;
    public readonly ushort Label = lbl;
    public readonly (Register Primary, Register Left, Register Right) Regs = (a, l, r);

    public override string ToString() => $"/{Kind}:{Label} {Regs.Primary} {Regs.Left} {Regs.Right}";
}

public sealed class NilaryInst(Register r, Port port) : Inst
{
    public readonly Register Reg = r;
    public readonly Port Value = port;

    public override string ToString() => $"/{Value} {Reg}";
}

public record struct Program
{
    public static Program Build(Action<Action<PortKind, ushort, int, int, int>, Action<Port, int>> build)
    {
        var insts = new List<Inst>();
        var maxReg = 0;

        void Binary(PortKind k, ushort lbl, int a, int l, int r)
        {
            insts.Add(new BinaryInst(k, lbl, new Register(a), new Register(l), new Register(r)));
            maxReg = int.Max(int.Max(maxReg, a), int.Max(l, r));
        }

        void Nilary(Port p, int r)
        {
            maxReg = int.Max(maxReg, r);
            insts.Add(new NilaryInst(new Register(r), p));
        }

        build(Binary, Nilary);
        return new Program
        {
            Registers = maxReg + 1,
            Instructions = insts.ToArray(),
        };
    }

    public int Registers;
    public Inst[] Instructions;

    public override string ToString() => string.Join('\n', Instructions.Select(x => x.ToString()));
}



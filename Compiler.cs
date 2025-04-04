namespace inet;

using Reg = Register;

public abstract class Expr
{
    public static Lit I32(int v) => new Lit(v);
    public static BinExpr Add(Expr l, Expr r) => new (BinOp.Add, l, r);
    public static IfExpr If(Expr c, Expr t, Expr e) => new (c, t, e);
}

public sealed class Lit(int v) : Expr
{
    public readonly int Value = v;
    public ulong Repr => (ulong)(((long)Value) & 0xFFFFFFFFL);
}

public sealed class IfExpr(Expr c, Expr t, Expr e) : Expr
{
    public readonly Expr Cond = c;
    public readonly Expr Then = t;
    public readonly Expr Else = e;
}

public enum BinOp { Add }
public sealed class BinExpr(BinOp op, Expr l, Expr r) : Expr
{
    public readonly BinOp Op = op;
    public readonly Expr Left = l;
    public readonly Expr Right = r;
}


public class Compiler
{
    public static Reg Root = new Reg(0);
    public int NextReg = 1;
    public List<Inst> Instructions = new List<Inst>();

    private const ushort Fn = 0;

    private const ushort TEMP_PRINT = 1;

    Reg Gen() => new Reg(NextReg++);

    public static Program CompileExpr(Expr e)
    {
        var c = new Compiler();
        var r = c.Gen();
        c.Compile(e, r);
        return c.Finish(r);
    }

    Program Finish(Reg exprVal)
    {
        // Wrap the expression in a lambda that calls print on the result:
        var ioArg = Gen();
        var retVal = Gen();
        Instructions.Add(new BinaryInst(PortKind.Comb, Fn, Root, ioArg, retVal));
        Instructions.Add(new BinaryInst(PortKind.ExtFn, TEMP_PRINT, ioArg, exprVal, retVal));

        return new Program
        {
            Registers = NextReg,
            Instructions = Instructions.ToArray(),
        };
    }

    void Compile(Expr e, Reg res)
    {
        if (e is Lit lit)
        {
            Instructions.Add(new NilaryInst(res, Port.FromExtVal(lit.Repr)));
            return;
        }

        if (e is BinExpr bine)
        {
            var l = Gen();
            var r = Gen();
            Instructions.Add(new BinaryInst(PortKind.ExtFn, Builtins.AddLabel, l, r, res));
            Compile(bine.Left, l);
            Compile(bine.Right, r);
            return;
        }

        if (e is IfExpr ife)
        {
            var c = Gen();
            var t = Gen();
            var f = Gen();
            var q = Gen();
            Instructions.Add(new BinaryInst(PortKind.Branch, 0, c, q, res));
            Instructions.Add(new BinaryInst(PortKind.Branch, 0, q, t, f));
            Compile(ife.Cond, c);
            Compile(ife.Then, t);
            Compile(ife.Else, f);
            return;
        }

        throw new NotImplementedException($"Unimplemented expr: {e}");
    }
}
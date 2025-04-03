namespace inet;

using Reg = Register;

public abstract class Expr
{
    public static Lit I32(int v) => new Lit(v);
    public static BinExpr Add(Expr l, Expr r) => new (BinOp.Add, l, r);
}

public sealed class Lit(int v) : Expr
{
    public readonly int Value = v;
    public ulong Repr => (ulong)(((long)Value) & 0xFFFFFFFFL);
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
            Compile(bine.Left, l);
            Compile(bine.Right, r);
            Instructions.Add(new BinaryInst(PortKind.ExtFn, Builtins.AddLabel, l, r, res));
            return;
        }

        throw new NotImplementedException($"Unimplemented expr: {e}");
    }
}
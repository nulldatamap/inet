namespace inet;

using Reg = Register;

public abstract class Expr
{
    public static Lit I32(int v) => new (v);
    public static Var Var(string v) => new (v);
    public static BinExpr Add(Expr l, Expr r) => new (BinOp.Add, l, r);
    public static IfExpr If(Expr c, Expr t, Expr e) => new (c, t, e);
    public static LetExpr Let(string n, Expr x, Expr body) => new (n, x, body);
    public static DoExpr Do(params Expr[] es) => new (es);

    public static PrintExpr Print(Expr io, Expr val) => new(io, val);
    // public static InlineExpr Inline(Func<Reg[], Inst[]> insts, params Expr[] es) => new bool(insts, es);
}

public sealed class Lit(int v) : Expr
{
    public readonly int Value = v;
    public ExtVal Repr => ExtVal.FromImm((ulong)(((long)Value) & 0xFFFFFFFFL));
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

public sealed class Var(string name) : Expr
{
    public readonly string Name = name;
}

public sealed class LetExpr(string var, Expr x, Expr body) : Expr
{
    public readonly string Var = var;
    public readonly Expr Val = x;
    public readonly Expr Body = body;
}

public sealed class DoExpr(Expr[] es) : Expr
{
    public readonly Expr[] Exprs = es;
}

public sealed class PrintExpr(Expr io, Expr val) : Expr
{
    public readonly Expr Io = io;
    public readonly Expr Val = val;
}

class Slot
{
    public Slot? Prev;
    public string Name;
    public Stack<Reg> Users;

    public Slot(string name)
    {
        Name = name;
        Users = new Stack<Reg>();
    }
}

class Scope
{
    public Scope? Parent;
    public Dictionary<string, Slot> Slots = new Dictionary<string, Slot>();

    public bool TryLookup(string name, out Slot slot)
    {
        if (Slots.TryGetValue(name, out slot))
            return true;

        return Parent?.TryLookup(name, out slot) ?? false;
    }

    public Slot Define(string name)
    {
        var newSlot = new Slot(name);
        if (Slots.TryGetValue(name, out var prevSlot))
        {
            newSlot.Prev = prevSlot;
        }
        Slots[name] = newSlot;
        return newSlot;
    }

    public void Undefine(string name)
    {
        var oldSlot = Slots[name];
        if (oldSlot.Prev != null)
            Slots[name] = oldSlot.Prev;
        else
            Slots.Remove(name);
    }
}

public class Compiler
{
    static Reg _root = new Reg(0);
    int _nextReg = 1;
    private int _dupCount = 0;
    List<Inst> _instructions = new List<Inst>();

    Scope _scope = new Scope();

    private const ushort Fn = 0;
    private const ushort Dup0 = 0x10;

    private const ushort TEMP_PRINT = 2;

    Reg Gen() => new Reg(_nextReg++);

    public static Program CompileExpr(Expr e)
    {
        var c = new Compiler();
        return c.Finish(e);
    }

    Program Finish(Expr e)
    {
        var ioArg = _scope.Define("io");
        var retVal = Gen();
        Compile(e, retVal);
        // Wrap the expression in a lambda that calls print on the result:
        var ioVal = MaterializeUsers(ioArg);
        _instructions.Add(new BinaryInst(PortKind.Comb, Fn, _root, ioVal, retVal));

        return new Program
        {
            Registers = _nextReg,
            Instructions = _instructions.ToArray(),
        };
    }

    void Dup(Reg x, Reg x0, Reg x1)
    {
        _instructions.Add(new BinaryInst(PortKind.Comb, (ushort)(Dup0 + _dupCount++), x, x0, x1));
    }

    Reg MaterializeUsers(Slot slot)
    {
        // Exactly one user, so just link directly
        if(slot.Users.Count == 1)
            return slot.Users.Pop();

        var src = Gen();
        if (slot.Users.Count > 1)
        {
            var from = src;
            // More than one user, so we need to duplicate the value
            while (slot.Users.Count > 0)
            {
                var user = slot.Users.Pop();
                var newFrom = slot.Users.Count == 1 ? slot.Users.Pop() : Gen();
                Dup(from, user, newFrom);
                from = newFrom;
            }

            return src;
        }

        // Variable is not used, so erase it
        _instructions.Add(new NilaryInst(src, Port.Eraser()));
        return src;
    }

    void Compile(Expr e, Reg res)
    {
        if (e is Lit lit)
        {
            // res = <lit>
            _instructions.Add(new NilaryInst(res, Port.FromExtVal(lit.Repr)));
            return;
        }

        if (e is BinExpr bine)
        {
            // l = <BINOP>(r res)
            var l = Gen();
            var r = Gen();
            _instructions.Add(new BinaryInst(PortKind.ExtFn, Builtins.AddLabel, l, r, res));
            Compile(bine.Left, l);
            Compile(bine.Right, r);
            return;
        }

        if (e is IfExpr ife)
        {
            // c = ?(?(t f) res)
            var c = Gen();
            var t = Gen();
            var f = Gen();
            var q = Gen();
            _instructions.Add(new BinaryInst(PortKind.Branch, 0, c, q, res));
            _instructions.Add(new BinaryInst(PortKind.Branch, 0, q, t, f));
            Compile(ife.Cond, c);
            Compile(ife.Then, t);
            Compile(ife.Else, f);
            return;
        }

        if (e is Var var)
        {
            if (_scope.TryLookup(var.Name, out var slot))
            {
                slot.Users.Push(res);
            }
            else
            {
                throw new InvalidOperationException($"Undefined variable `{var.Name}`");
            }

            return;
        }

        if (e is LetExpr let)
        {
            var varSlot = _scope.Define(let.Var);
            Compile(let.Body, res);
            Compile(let.Val, MaterializeUsers(varSlot));
            return;
        }

        if (e is DoExpr doe)
        {
            if (doe.Exprs.Length == 0)
            {
                _instructions.Add(new NilaryInst(res, Port.FromExtVal(ExtVal.FromImm(0))));
            } else if (doe.Exprs.Length == 1)
            {
                Compile(doe.Exprs[0], res);
            }
            else
            {
                var prevStep = Gen();
                Compile(doe.Exprs[0], prevStep);
                for (int i = 1; i < doe.Exprs.Length; i++)
                {
                    var thisStep = Gen();
                    Compile(doe.Exprs[i], thisStep);
                    var nextStep = i == doe.Exprs.Length - 1 ? res : Gen();
                    _instructions.Add(new BinaryInst(PortKind.ExtFn, Builtins.SeqLabel, prevStep, thisStep, nextStep));
                    prevStep = nextStep;
                }
            }
            return;
        }

        if (e is PrintExpr print)
        {
            var io = Gen();
            var val = Gen();
            Compile(print.Io, io);
            Compile(print.Val, val);
            _instructions.Add(new BinaryInst(PortKind.ExtFn, TEMP_PRINT, io, val, res));
            return;
        }

        throw new NotImplementedException($"Unimplemented expr: {e}");
    }
}
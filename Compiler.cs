namespace inet;

using Reg = Register;

public abstract class Expr
{
    public static Lit I32(int v) => new (v);
    public static Var Var(string v) => new (v);
    public static BinExpr Add(Expr l, Expr r) => new (BinOp.Add, l, r);
    public static IfExpr If(Expr c, Expr t, Expr e) => new (c, t, e);
    public static LetExpr Let(string n, Expr x, Expr body) => new (n, x, body);
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

    private const ushort TEMP_PRINT = 1;

    Reg Gen() => new Reg(_nextReg++);

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
        _instructions.Add(new BinaryInst(PortKind.Comb, Fn, _root, ioArg, retVal));
        _instructions.Add(new BinaryInst(PortKind.ExtFn, TEMP_PRINT, ioArg, exprVal, retVal));

        return new Program
        {
            Registers = _nextReg,
            Instructions = _instructions.ToArray(),
        };
    }

    public void Dup(Reg x, Reg x0, Reg x1)
    {
        _instructions.Add(new BinaryInst(PortKind.Comb, (ushort)(Dup0 + _dupCount++), x, x0, x1));
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

            if (varSlot.Users.Count == 0)
            {
                // Variable is not used, so erase it
                var varOrigin = Gen();
                Compile(let.Val, varOrigin);
                _instructions.Add(new NilaryInst(varOrigin, Port.Eraser()));
            } else if (varSlot.Users.Count > 1)
            {
                // More than one user, so we need to duplicate the value
                var src = Gen();
                Compile(let.Val, src);

                while (varSlot.Users.Count > 0)
                {
                    var user = varSlot.Users.Pop();
                    var newSrc = varSlot.Users.Count == 1 ? varSlot.Users.Pop() : Gen();
                    Dup(src, user, newSrc);
                    src = newSrc;
                }
            }
            else
            {
                // Exactly one user, so just link directly
                Compile(let.Val, varSlot.Users.Pop());
            }

            return;
        }

        throw new NotImplementedException($"Unimplemented expr: {e}");
    }
}
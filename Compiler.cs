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
    public static CallExpr Call(Expr fn, params Expr[] es) => new (fn, es);
    public static LamExpr Lam(string[] prms, Expr b) => new (prms, b);

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

public sealed class CallExpr(Expr fn, Expr[] args) : Expr
{
    public readonly Expr Fn = fn;
    public readonly Expr[] Args = args;
}

public sealed class LamExpr(string[] prms, Expr b) : Expr
{
    public readonly string[] Params = prms;
    public readonly Expr Body = b;
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

class Scope(Scope? p = null)
{
    public Scope? Parent = p;
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
    private static Reg _root = new Reg(0);
    private int _nextReg = 1;
    private int _dupCount = 0;
    private List<Inst> _instructions = new List<Inst>();

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

    void Erase(Reg x)
    {
        _instructions.Add(new NilaryInst(x, Port.Eraser()));
    }
    
    void Val(Reg x, ExtVal v)
    {
        _instructions.Add(new NilaryInst(x, Port.FromExtVal(v)));
    }

    void ExtFn(ushort label, Reg l, Reg r, Reg res)
    {
        _instructions.Add(new BinaryInst(PortKind.ExtFn, label, l, r, res));
    }

    void Branch(Reg c, Reg t, Reg f, Reg res)
    {
        // c = (? (? t f) res)
        var q = Gen();
        _instructions.Add(new BinaryInst(PortKind.Branch, 0, c, q, res));
        _instructions.Add(new BinaryInst(PortKind.Branch, 0, q, t, f));
    }

    void App(Reg fn, Reg arg, Reg res)
    {
        _instructions.Add(new BinaryInst(PortKind.Comb, Fn, fn, arg, res));
    }
    
    void Lam(Reg prm, Reg body, Reg fn)
    {
        _instructions.Add(new BinaryInst(PortKind.Comb, Fn, fn, prm, body));
    }

    Reg MaterializeUsers(Slot slot)
    {
        // Exactly one user, so just link directly
        if(slot.Users.Count == 1)
            return slot.Users.Pop();

        var src = Gen();
        if (slot.Users.Count > 1)
        {
            // More than one user, so we need to duplicate the value
            var from = src;
            // src = dup(u0 dup(u1 dup(u2 u3)))
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
        Erase(src);
        return src;
    }

    Reg Compile(Expr e)
    {
        var res = Gen();
        Compile(e, res);
        return res;
    }
    
    void Compile(Expr e, Reg res)
    {
        if (e is Lit lit)
        {
            // res = <lit>
            Val(res, lit.Repr);
            return;
        }

        if (e is BinExpr bine)
        {
            // l = <BINOP>(r res)
            ExtFn(Builtins.AddLabel,
                Compile(bine.Left),
                Compile(bine.Right),
                res);
            return;
        }

        if (e is IfExpr ife)
        {
            // c = ?(?(t f) res)
            Branch(
                Compile(ife.Cond),
                Compile(ife.Then),
                Compile(ife.Else),
                res);
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
            _scope.Undefine(let.Var);
            Compile(let.Val, MaterializeUsers(varSlot));
            return;
        }

        if (e is DoExpr doe)
        {
            if (doe.Exprs.Length == 0)
            {
                Val(res, ExtVal.Nil);
            } else if (doe.Exprs.Length == 1)
            {
                Compile(doe.Exprs[0], res);
            }
            else
            {
                // SEQ(SEQ(SEQ(e3 e2) e1) e0)
                var prevStep = Gen();
                Compile(doe.Exprs[0], prevStep);
                for (int i = 1; i < doe.Exprs.Length; i++)
                {
                    var nextStep = i == doe.Exprs.Length - 1 ? res : Gen();
                    ExtFn(Builtins.SeqLabel,
                        prevStep,
                        Compile(doe.Exprs[i]),
                        nextStep);
                    prevStep = nextStep;
                }
            }
            return;
        }

        if (e is CallExpr call)
        {
            var fnVal = Gen();
            Compile(call.Fn, fnVal);

            // If we're calling a function that doesn't have any parameters, we pass in a dummy argument
            if (call.Args.Length == 0)
            {
                var nil = Gen();
                Val(nil, ExtVal.Nil);
                App(fnVal, nil, res);
                return;
            }

            var callee = fnVal;
            // <fn> = fn(x0 fn(x1 fn(x2 res)))
            for(int i = 0; i < call.Args.Length; i++)
            {
                var ret = i == call.Args.Length - 1 ? res : Gen();
                App(callee, Compile(call.Args[i]), ret);
                callee = ret;
            }
            
            return;
        }

        if (e is LamExpr lam)
        {
            _scope = new Scope(_scope);
            var slots = lam.Params.Select(prm => _scope.Define(prm)).ToArray();
            var body = Compile(lam.Body);
            _scope = _scope.Parent;

            // If the lambda takes no arguments, put in a dummy one
            if (slots.Length == 0)
            {
                var nil = Gen();
                Val(nil, ExtVal.Nil);
                Lam(nil, body, res);
                return;
            }

            var head = res;
            // res = fn(x0 fn(x1 (fn x2 body)))
            for(int i = 0; i < slots.Length; i++)
            {
                var isLast = i == slots.Length - 1;
                var inner = isLast ? body : Gen();
                Lam(MaterializeUsers(slots[i]), inner, head);
                head = inner;
            }
            
            return;
        }

        if (e is PrintExpr print)
        {
            ExtFn(TEMP_PRINT,
                Compile(print.Io),
                Compile(print.Val),
                res);
            return;
        }

        throw new NotImplementedException($"Unimplemented expr: {e}");
    }
}
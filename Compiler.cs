using System.Diagnostics;

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
    public static TupExpr Tup(params Expr[] es) => new (es);
    public static UntupExpr Untup(string[] xs, Expr t, Expr b) => new (xs, t, b);
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

public sealed class TupExpr(params Expr[] es) : Expr
{
    public readonly Expr[] Items = es;
}

public sealed class UntupExpr(string[] xs, Expr tup, Expr body) : Expr
{
    public readonly string[] Vars = xs;
    public readonly Expr Tup = tup;
    public readonly Expr Body = body;
}

public class Def(string name, Expr body)
{
    public readonly string Name = name;
    public readonly Expr Body = body;
}

public class Module(Def[] defs)
{
    public Def[] Defs = defs;
}

class Slot
{
    public Slot? Prev;
    public string Name;
    public List<Reg> Users;

    public Slot(string name)
    {
        Name = name;
        Users = new List<Reg>();
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

class Global(string name, ulong index, Expr body)
{
    public readonly ulong Index = index;
    public readonly string Name = name;
    public readonly Expr Body = body;
}

public class Compiler
{
    private static Reg _root = new Reg(0);
    private int _nextReg = 1;
    private int _dupCount = 0;
    private List<Inst> _instructions = new List<Inst>();

    Dictionary<string, Global> _globals = new Dictionary<string, Global>();

    Scope _scope = new Scope();

    private const ushort Fn = 0;
    private const ushort Constr = 1;
    private const ushort Dup0 = 2;

    private const ushort TEMP_PRINT = 2;

    Reg Gen() => new Reg(_nextReg++);

    public static Program CompileExpr(Expr e)
    {
        var c = new Compiler();
        return c.Finish(e);
    }

    public static Program[] Compile(Module m)
    {
        var c = new Compiler();
        return c.CompileModule(m);
    }

    void Reset()
    {
        _nextReg = 1;
        _instructions.Clear();
        _scope = new Scope();
    }

    Program[] CompileModule(Module m)
    {
        foreach (var def in m.Defs)
            _globals[def.Name] = new Global(def.Name, (ulong)_globals.Count, def.Body);

        return _globals.Values.Select(global =>
        {
            Reset();
            var prog = Finish(global.Body);
            Console.WriteLine($"::{global.Name}\n{prog}");
            return prog;
        }).ToArray();
    }

    Program Finish(Expr e)
    {
        Compile(e, _root);
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
        _instructions.Add(new BinaryInst(PortKind.Operator, (ushort)OperatorKind.Branch, c, q, res));
        _instructions.Add(new BinaryInst(PortKind.Operator, (ushort)OperatorKind.Branch, q, t, f));
    }

    void App(Reg fn, Reg arg, Reg res)
    {
        _instructions.Add(new BinaryInst(PortKind.Comb, Fn, fn, arg, res));
    }

    void Lam(Reg prm, Reg body, Reg fn)
    {
        _instructions.Add(new BinaryInst(PortKind.Comb, Fn, fn, prm, body));
    }

    void Tup(Reg l, Reg r, Reg res)
    {
        _instructions.Add(new BinaryInst(PortKind.Comb, Constr, l, r, res));
    }

    void RightChain<L, T>(Reg head, L xs, Func<T, Reg> f, Action<Reg, Reg, Reg> g, Func<Reg>? final = null) where L: IList<T>
    {
        // x0..x3 = map f xs
        if (final == null)
        {
            // head = <g>(x0 <g>(x1 <g>(x2 x3)))
            for(int i = 0; i < xs.Count - 1; i++)
            {
                var right = i == xs.Count - 2 ? f(xs[^1]) : Gen();
                g(head, f(xs[i]), right);
                head = right;
            }
        }
        else
        {
            // head = <g>(x0 <g>(x1 <g>(x2 final)))
            for (int i = 0; i < xs.Count; i++)
            {
                var right = i == xs.Count - 1 ? final() : Gen();
                g(f(xs[i]), right, head);
                head = right;
            }
        }
    }

    Reg MaterializeUsers(Slot slot)
    {
        // Exactly one user, so just link directly
        if(slot.Users.Count == 1)
            return slot.Users[0];

        var src = Gen();
        if (slot.Users.Count > 1)
        {
            // Respect the side effect order
            slot.Users.Reverse();
            // More than one user, so we need to duplicate the value
            // src = dup(u0 dup(u1 dup(u2 u3)))
            RightChain<List<Reg>, Reg>(src, slot.Users, x => x, Dup);
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
                slot.Users.Add(res);
            }
            else if (_globals.TryGetValue(var.Name, out var global))
            {
                _instructions.Add(new NilaryInst(res, Port.Global(global.Index)));
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

            // <fn> = fn(x0 fn(x1 fn(x2 res)))
            RightChain<Expr[], Expr>(fnVal, call.Args, Compile, (arg, ret, callee) =>
            {
                App(callee, arg, ret);
            }, () => res);
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

            // res = fn(x0 fn(x1 (fn x2 body)))
            RightChain<Slot[], Slot>(res, slots, MaterializeUsers, Lam, () => body);
            return;
        }

        if (e is TupExpr tupe)
        {
            Debug.Assert(tupe.Items.Length >= 2);
            // res = tup(e0 tup(e1 tup(e2 e3)))
            RightChain<Expr[], Expr>(res, tupe.Items, Compile, Tup);
            return;
        }

        if (e is UntupExpr untupe)
        {
            Debug.Assert(untupe.Vars.Length >= 2);
            _scope = new Scope(_scope);
            var slots = untupe.Vars.Select(prm => _scope.Define(prm)).ToArray();
            Compile(untupe.Body, res);
            _scope = _scope.Parent;

            // tup(e0 tup(e1 tup(e2 e3))) = t
            RightChain<Slot[], Slot>(Compile(untupe.Tup), slots, MaterializeUsers, Tup);
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
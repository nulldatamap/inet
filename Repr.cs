using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace inet;

public readonly record struct Wire
{
    private readonly ulong _value;

    public Wire(ulong raw)
    {
        _value = raw;
    }

    public unsafe ulong* Raw => (ulong*)_value;

    public Port Load()
    {
        unsafe
        {
            return new Port(*Raw);
        }
    }

    public void Set(Port p)
    {
        unsafe
        {
            *Raw = p.RawValue;
        }
    }

    public Port Swap(Port p)
    {
        unsafe
        {
            var old = *Raw;
            *Raw = p.RawValue;
            return new Port(old);
        }
    }

    public Port ToPort()
    {
        unsafe
        {
            return new Port(PortKind.Wire, 0, _value);
        }
    }

    public override string ToString()
    {
        return $"W:{_value:X08}";
    }
}

public enum PortKind
{
    Unassigned,
    ExtVal,
    ExtFn,
    Global,
    Comb,
    Branch,
    Wire,
    Eraser,
}

public struct Cell<T> where T : unmanaged
{
    private long _count;
    private T _value;

    public Cell(T v)
    {
        _value = v;
        _count = 1;
    }

    public unsafe T* AsPointer() => (T*)Unsafe.AsPointer(ref _value);

    public T Value
    {
        get
        {
            Debug.Assert(_count > 0);
            return _value;
        }
        set
        {
            Debug.Assert(_count > 0);
            _value = value;
        }
    }

    public long RefCount => _count;

    public void Increment()
    {
        var v = Interlocked.Increment(ref _count);
        Debug.Assert(v >= 1);
    }

    public void Decrement()
    {
        var v = Interlocked.Decrement(ref _count);
        Debug.Assert(v >= 0);
    }
}

public readonly struct ExtVal
{
    private const ulong CELL_BIT = 0b1000;
    private const int IMM_SHIFT = 4;

    private readonly ulong _value;

    public static readonly ExtVal Nil = default;

    ExtVal(ulong v)
    {
        Debug.Assert((v & 0b111) == 0);
        _value = v;
    }

    public static ExtVal FromImm(ulong x)
    {
        var imm = x << IMM_SHIFT;
        Debug.Assert((imm >> IMM_SHIFT) == x);
        return new ExtVal(imm);
    }

    public static unsafe ExtVal FromRef<T>(ref Cell<T> c) where T: unmanaged
    {
        var addr = (ulong)c.AsPointer();
        Debug.Assert((addr & CELL_BIT) != 0);
        return new ExtVal(addr);
    }

    public static ExtVal FromRaw(ulong x)
    {
        return new ExtVal(x);
    }

    public ulong Raw => _value;

    public bool IsImm => (_value & CELL_BIT) == 0;
    public bool IsRef => !IsImm;

    public bool IsTruthy => _value != 0;

    public ulong Imm
    {
        get
        {
            Debug.Assert((_value & CELL_BIT) == 0);
            return _value >> IMM_SHIFT;
        }
    }

    public unsafe ref Cell<T> Ref<T>() where T: unmanaged
    {
        Debug.Assert((_value & CELL_BIT) != 0);
        return ref Unsafe.AsRef<Cell<T>>(&((ulong*)_value)[-1]);
    }

    public ExtVal Dup()
    {
        if (IsRef)
            Ref<ulong>().Increment();
        return this;
    }

    public void Drop()
    {
        if (IsRef)
            Ref<ulong>().Decrement();
    }

    public override string ToString()
    {
        if (IsImm)
        {
            return $"{Imm}";
        }

        var c = Ref<ulong>();
        return $"@[{c.RefCount}]{_value:X08}";
    }
}

public record struct Port
{
    private const ulong KIND_MASK = 0b111;
    private const int KIND_SHIFT = 3;
    private const int INDEX_SHIFT = 48;
    private const ulong LABEL_MASK = 0xFFFFul << INDEX_SHIFT;
    private const ulong ADDR_MASK = ~(LABEL_MASK | KIND_MASK);
    private const ushort SWAP_BIT = 0x8000;

    private readonly ulong _value = 0;

    public ulong RawValue => _value;

    public PortKind Kind => (PortKind)(_value & KIND_MASK);
    public ushort Label => (ushort)(_value >> INDEX_SHIFT);
    public ushort SwappedLabel => (ushort)(Label ^ SWAP_BIT);
    public ulong Addr => _value & ADDR_MASK;
    public ExtVal ExtVal => ExtVal.FromRaw(_value & ~KIND_MASK);

    public static Port Eraser() => new Port(PortKind.Eraser, 0, 0);
    public static Port FromExtVal(ExtVal val) => new Port(val.Raw | (ulong)PortKind.ExtVal);
    public static Port Global(ulong globalId) => new Port(PortKind.Global, 0, globalId);

    public bool IsAssigned
    {
        get
        {
            if (Kind == PortKind.Unassigned)
                Debug.Assert(_value == 0);
            return Kind != PortKind.Unassigned;
        }
    }

    public bool IsFreeNode => Kind == PortKind.Unassigned && _value != 0;

    public static bool IsBinaryKind(PortKind k) => k is PortKind.Branch or PortKind.Comb or PortKind.ExtFn;
    public static bool IsNilaryKind(PortKind k) => k is PortKind.Eraser or PortKind.Global or PortKind.ExtVal;

    public bool IsBinary => IsBinaryKind(Kind);
    public bool IsNilary => IsNilaryKind(Kind);

    public Port Dup()
    {
        if (Kind == PortKind.ExtVal)
            return FromExtVal(ExtVal.Dup());
        return this;
    }

    public void Drop()
    {
        if (Kind == PortKind.ExtVal)
            ExtVal.Drop();
    }

    public (Wire Left, Wire Right) Aux
    {
        get
        {
            Debug.Assert(IsBinary);
            var addr = _value & ADDR_MASK;
            return (new Wire(addr), new Wire(addr + sizeof(ulong)));
        }
    }

    public Wire Wire
    {
        get
        {
            Debug.Assert(Kind == PortKind.Wire);
            return new Wire(_value & ADDR_MASK);
        }
    }

    public (int FnId, bool Swapped) GetExtFn()
    {
        Debug.Assert(Kind == PortKind.ExtFn);
        return (Label & ~SWAP_BIT, (Label & SWAP_BIT) != 0);
    }


    public Port(PortKind kind, ushort index, ulong value)
    {
        Debug.Assert((value & KIND_MASK) == 0);
        Debug.Assert((value & LABEL_MASK) == 0
                     || (value & LABEL_MASK) == 0xFF);
        _value = (ulong)kind | value | (((ulong) index) << INDEX_SHIFT);
        Debug.Assert(Addr == value);
    }

    public Port(ulong raw)
    {
        _value = raw;
    }

    public override string ToString()
    {
        switch (Kind)
        {
            case PortKind.Unassigned:
                return _value != 0 ? "<FREE>" : "-";
            case PortKind.Comb:
                return $"C:{Label:X02}:{_value&ADDR_MASK:X08}";
            case PortKind.Branch:
                return $"B:{_value&ADDR_MASK:X08}";
            case PortKind.ExtVal:
                return $"EXT:{ExtVal:X02}";
            case PortKind.ExtFn:
                return $"EFN:{Label:X02}:{Addr:X08}";
            case PortKind.Global:
                return $"G:{ExtVal:X02}";
            case PortKind.Eraser:
                return "_";
            case PortKind.Wire:
                return Wire.ToString();
            default:
                return "<unknown>";
        }
    }
}


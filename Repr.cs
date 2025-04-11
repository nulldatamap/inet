using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

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
    Operator,
    Wire,
    Eraser,
}

public enum OperatorKind
{
    Branch,
    Lift,
    Lower,
    _Count,
}

interface IRefCounted
{
    public void Increment();
    public bool Decrement();
}

public struct Cell<T> : IRefCounted where T : unmanaged
{
    private long _count;
    private T _value;

    internal Cell(T v)
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

    public bool Decrement()
    {
        var v = Interlocked.Decrement(ref _count);
        return v == 0;
    }
}

public struct Slots : IRefCounted
{
    private uint _count;
    private uint _length;

    internal Slots(uint length)
    {
        _count = 1;
        _length = length;
    }

    public unsafe ExtVal* AsPointer() => (ExtVal*)(&((ulong*)Unsafe.AsPointer(ref _length))[1]);
    public unsafe Span<ExtVal> AsSpan() => new Span<ExtVal>(AsPointer(), (int)_length);

    public long RefCount => _count;

    public void Increment()
    {
        var v = Interlocked.Increment(ref _count);
        Debug.Assert(v >= 1);
    }

    public bool Decrement()
    {
        var v = Interlocked.Decrement(ref _count);
        var shouldFree = v == 0;
        if (shouldFree)
        {
            var span = AsSpan();
            for (int i = 0; i < span.Length; i++)
            {
                span[i].Drop();
            }
        }

        return shouldFree;
    }
}

public enum ExtTyFlags
{
    None = 0b00,
    RefCounted = 0b01,
    DupDropOverload = 0b10,

    Imm = None,
    Ref = RefCounted,
    Uniq = RefCounted | DupDropOverload,
}

public readonly record struct ExtTyDesc(
    string Name,
    bool Immediate,
    uint Size = 0,
    bool Dynamic = false,
    ExtTyDesc.DupF? Dup = null,
    ExtTyDesc.DropF Drop = null)
{
    public delegate void DupF(ref ExtVal self);

    public delegate bool DropF(ref ExtVal self);

    public static ExtTyDesc Imm(string name)
        => new ExtTyDesc(name, Immediate: true);

    public static ExtTyDesc Ref<T>(string name) where T : unmanaged
        => new ExtTyDesc(name, Immediate: false, Size: (uint)Unsafe.SizeOf<T>());
}

public readonly record struct ExtTy
{
    private const ushort FLAG_MASK = 0xc000;
    private const int FLAG_SHIFT = 14;

    private readonly ushort _value;

    public ExtTy(ExtTyFlags flags, ushort id)
    {
        _value = (ushort)(id | (((ushort)flags) << FLAG_SHIFT));
        Debug.Assert((_value & ~FLAG_MASK) == id);
    }

    private ExtTy(ushort value) => _value = value;

    public static ExtTy FromRaw(ushort v)
    {
        return new ExtTy(v);
    }

    public ushort TypeIndex => (ushort)(_value & ~FLAG_MASK);
    public ExtTyFlags Flags => (ExtTyFlags)(_value >> FLAG_SHIFT);
    public ushort Raw => _value;
}

public readonly struct ExtVal
{
    private const int IMM_SHIFT = 4;
    private const int TY_SHIFT = 48;
    private const ulong VALUE_MASK = 0x0000FFFF_FFFFFFFF;

    private readonly ulong _value;

    public static readonly ExtVal Nil = default;

    ExtVal(ExtTy ty, ulong v)
    {
        Debug.Assert((v & 0b111) == 0);
        _value = v | ((ulong)ty.Raw << TY_SHIFT);
    }

    ExtVal(ulong raw)
    {
        Debug.Assert((raw & 0b111) == 0);
        _value = raw;
    }

    public static ExtVal FromImm(ExtTy ty, ulong x)
    {
        var imm = x << IMM_SHIFT;
        Debug.Assert((imm >> IMM_SHIFT) == x);
        Debug.Assert(ty.Flags == ExtTyFlags.Imm);
        return new ExtVal(ty, imm);
    }

    public static unsafe ExtVal MakeRef<T>(ExtTy ty, T v, nuint extraPayloadSize = 0) where T: unmanaged
    {
        var ptr = (Cell<T>*)NativeMemory.AlignedAlloc((nuint)Unsafe.SizeOf<Cell<T>>() + extraPayloadSize, 16);
        *ptr = new Cell<T>(v);
        Debug.Assert(ty.Flags == ExtTyFlags.Ref);
        return new ExtVal(ty, (ulong)ptr);
    }

    public static unsafe ExtVal MakeUniq<T>(ExtTy ty, T v, nuint extraPayloadSize = 0) where T: unmanaged
    {
        var ptr = (T*)NativeMemory.AlignedAlloc((nuint)Unsafe.SizeOf<T>() + extraPayloadSize, 16);
        Debug.Assert(ty.Flags == ExtTyFlags.Uniq);
        return new ExtVal(ty, (ulong)ptr);
    }

    private static unsafe ref Slots AllocArray(uint count, bool zeroUnit)
    {
        var ptr = (Slots*)NativeMemory.AlignedAlloc((nuint)(Unsafe.SizeOf<Slots>() + Unsafe.SizeOf<ExtVal>() * count), 16);
        if (zeroUnit)
        {
            for (var i = 0; i < count; i++) ptr->AsSpan()[i] = Nil;
        }
        return ref Unsafe.AsRef<Slots>(ptr);
    }

    // public static unsafe ExtVal MakeArray(ExtTy ty, uint length)
    // {
    //     Debug.Assert(ty.Flags == ExtTyFlags.Array);
    //     ref var slots = ref AllocArray(length, zeroUnit: false);
    //     return new ExtVal(ty, (ulong)Unsafe.AsPointer(ref slots));
    // }

    public static ExtVal FromRaw(ulong x)
    {
        return new ExtVal(x);
    }

    public ulong Raw => _value;

    public bool IsImm => Type.Flags == ExtTyFlags.Imm;
    public bool IsRef => Type.Flags == ExtTyFlags.Ref;
    public bool IsUniq => Type.Flags == ExtTyFlags.Uniq;
    public ExtTy Type => ExtTy.FromRaw((ushort)(_value >> TY_SHIFT));

    public bool IsTruthy => (_value & VALUE_MASK) != 0;

    public ulong Imm
    {
        get
        {
            Debug.Assert(IsImm);
            return _value >> IMM_SHIFT;
        }
    }

    unsafe void* GetPointer() => (void*)(_value & VALUE_MASK);

    public unsafe ref Cell<T> Ref<T>() where T: unmanaged
    {
        Debug.Assert(IsRef);
        return ref Unsafe.AsRef<Cell<T>>(GetPointer());
    }

    public ExtVal Dup(ref Rt rt)
    {
        if (Type.Flags.HasFlag(ExtTyFlags.DupDropOverload))
        {
            var self = this;
            var desc = rt.GetTypeDesc(Type);
            desc.Dup(ref self);
            return self;
        }

        if (IsRef)
            Ref<ulong>().Increment();
        return this;
    }

    public void Drop(ref Rt rt)
    {
        unsafe
        {
            if (Type.Flags.HasFlag(ExtTyFlags.DupDropOverload))
            {
                var self = this;
                var desc = rt.GetTypeDesc(Type);
                if (desc.Drop(ref self))
                    NativeMemory.AlignedFree(GetPointer());
                return;
            }
            if (IsRef)
            {
                if (Ref<ulong>().Decrement())
                    NativeMemory.AlignedFree(GetPointer());
            }
        }
    }

    public override string ToString()
    {
        if (IsImm)
        {
            return $"{Imm}";
        }

        if (IsUniq)
            return $@"{Type.Raw:X04}[*]{_value:X08}";

        ref var c = ref Ref<ulong>();
        return $"@{Type.Raw:X04}[{c.RefCount}]{_value:X08}";
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
    public ulong GlobalIndex => _value >> KIND_SHIFT;
    public ExtVal ExtVal => ExtVal.FromRaw(_value & ~KIND_MASK);

    public static Port Eraser() => new Port(PortKind.Eraser, 0, 0);
    public static Port FromExtVal(ExtVal val) => new Port(val.Raw | (ulong)PortKind.ExtVal);
    public static Port Global(ulong globalId) => new Port(PortKind.Global, 0, globalId << KIND_SHIFT);

    public OperatorKind Operator
    {
        get
        {
            var l = Label;
            Debug.Assert(Kind == PortKind.Operator);
            Debug.Assert(Label < (int)OperatorKind._Count);
            return (OperatorKind)l;
        }
    }

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

    public static bool IsBinaryKind(PortKind k) => k is PortKind.Operator or PortKind.Comb or PortKind.ExtFn;
    public static bool IsNilaryKind(PortKind k) => k is PortKind.Eraser or PortKind.Global or PortKind.ExtVal;

    public bool IsBinary => IsBinaryKind(Kind);
    public bool IsNilary => IsNilaryKind(Kind);

    public Port Dup(ref Rt rt)
    {
        if (Kind == PortKind.ExtVal)
            return FromExtVal(ExtVal.Dup(ref rt));
        return this;
    }

    public void Drop(ref Rt rt)
    {
        if (Kind == PortKind.ExtVal)
            ExtVal.Drop(ref rt);
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
            case PortKind.Operator:
                return $"OP:{Label:X02}:{_value&ADDR_MASK:X08}";
            case PortKind.ExtVal:
                return $"EXT:{ExtVal:X02}";
            case PortKind.ExtFn:
                return $"EFN:{Label:X02}:{Addr:X08}";
            case PortKind.Global:
                return $"G:{GlobalIndex:X02}";
            case PortKind.Eraser:
                return "_";
            case PortKind.Wire:
                return Wire.ToString();
            default:
                return "<unknown>";
        }
    }
}


using System.Diagnostics;

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
    Wire,
    Eraser,
    Free,
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
    public ulong ExtVal => (_value & ~KIND_MASK) >> KIND_SHIFT;

    public static Port Eraser() => new Port(PortKind.Eraser, 0, 0);
    public static unsafe Port Free(ulong* next) => new Port(PortKind.Free, 0, (ulong)next);
    public static Port FromExtVal(ulong val) => new Port((val << KIND_SHIFT) | (ulong)PortKind.ExtVal);
    public static Port Global(ulong globalId) => new Port(PortKind.Global, 0, globalId);

    public bool IsAssigned => Kind is not (PortKind.Unassigned or PortKind.Free);


    public (Wire Left, Wire Right) Aux
    {
        get
        {
            Debug.Assert(Kind is PortKind.Comb or PortKind.ExtFn);
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
                return "-";
            case PortKind.Comb:
                return $"C:{Label:X02}:{_value&ADDR_MASK:X08}";
            case PortKind.ExtVal:
                return $"EXT:{ExtVal:X02}";
            case PortKind.ExtFn:
                return $"EFN:{Label:X02}:{Addr:X08}";
            case PortKind.Global:
                return $"G:{ExtVal:X02}";
            case PortKind.Eraser:
                return "_";
            case PortKind.Free:
                return "<FREE>";
            case PortKind.Wire:
                return Wire.ToString();
            default:
                return "<unknown>";
        }
    }
}


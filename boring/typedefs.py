from dataclasses import dataclass, field
import enum
from typing import List, Dict, Optional, Union


class IntBitness(enum.Enum):
    X8 = "X8"
    X16 = "X16"
    X32 = "X32"
    X64 = "X64"
    X128 = "X128"


class Signedness(enum.Enum):
    Signed = "Signed"
    Unsigned = "Unsigned"


class FloatBitness(enum.Enum):
    X32 = "X32"
    X64 = "X64"
    X128 = "X128"


@dataclass
class IntTypeDef:
    signedness: Signedness
    bitness: IntBitness


@dataclass
class FloatTypeDef:
    bitness: FloatBitness


@dataclass
class FunctionTypeDef:
    arguments: List["TypeDef"]
    return_type: "TypeDef"


@dataclass
class StructTypeDef:
    fields: Dict[str, "TypeDef"]


@dataclass
class UnitTypeDef:
    pass


@dataclass
class NeverTypeDef:
    pass


TypeDef = Union[
    IntTypeDef, FloatTypeDef, FunctionTypeDef, StructTypeDef, UnitTypeDef, NeverTypeDef
]


builtins: Dict[str, TypeDef] = {
    "U8": IntTypeDef(Signedness.Unsigned, IntBitness.X8),
    "U16": IntTypeDef(Signedness.Unsigned, IntBitness.X16),
    "U32": IntTypeDef(Signedness.Unsigned, IntBitness.X32),
    "U64": IntTypeDef(Signedness.Unsigned, IntBitness.X64),
    "U128": IntTypeDef(Signedness.Unsigned, IntBitness.X128),
    "I8": IntTypeDef(Signedness.Signed, IntBitness.X8),
    "I16": IntTypeDef(Signedness.Signed, IntBitness.X16),
    "I32": IntTypeDef(Signedness.Signed, IntBitness.X32),
    "I64": IntTypeDef(Signedness.Signed, IntBitness.X64),
    "I128": IntTypeDef(Signedness.Signed, IntBitness.X128),
    "F32": FloatTypeDef(FloatBitness.X32),
    "F64": FloatTypeDef(FloatBitness.X64),
    "F128": FloatTypeDef(FloatBitness.X128),
    "()": UnitTypeDef(),
    "!": NeverTypeDef(),
}

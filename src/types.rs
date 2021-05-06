#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Signedness {
    Signed,
    Unsigned,
}


#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum IntBitness {
    X8,
    X16,
    X32,
    X64,
    X128,
}


#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum FloatBitness {
    X32,
    X64,
    X128,
}


#[derive(Clone, Eq, PartialEq, Hash)]
pub struct IntTypeDef {
    pub signedness: Signedness,
    pub bitness: IntBitness,
}


#[derive(Clone, PartialEq, Eq, Hash)]
pub struct FloatTypeDef {
    pub bitness: FloatBitness,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct FunctionTypeDef {
    pub arguments: Vec<Type>,
    pub return_type: Box<Type>,
}


#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Bool,
    Int(IntTypeDef),
    Float(FloatTypeDef),
    Function(FunctionTypeDef),
    // String(StringTypeDef),
    // Struct(StructTypeDef),
    // Trait(TraitTypeDef),
    // Void,
    // Never,
}


/// Used for places where type info may or may not be solved.
#[derive(Clone, Eq, PartialEq, Hash)]
pub enum SpecifiedType {
    Unknown,
    Type(Type),
}

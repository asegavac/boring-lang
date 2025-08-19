export interface Spanned {
  spanStart: number;
  spanEnd: number;
}

export interface Identifier extends Spanned {
  name: string;
}

export interface LiteralInt {
  expressionType: "LiteralInt";
  value: string;
  type: TypeUsage;
}

export interface LiteralFloat {
  expressionType: "LiteralFloat";
  value: string;
  type: TypeUsage;
}

export interface LiteralBool {
  expressionType: "LiteralBool";
  value: string;
  type: TypeUsage;
}

export interface LiteralString {
  expressionType: "LiteralString";
  value: string;
  type: TypeUsage;
}

export interface StructField {
  name: Identifier;
  expression: Expression;
}

export interface LiteralStruct {
  expressionType: "LiteralStruct";
  name: Identifier;
  fields: StructField[];
  type: TypeUsage;
}

export interface FunctionCall {
  expressionType: "FunctionCall";
  source: Expression;
  arguments: Expression[];
  type: TypeUsage;
}

export interface StructGetter {
  expressionType: "StructGetter";
  source: Expression;
  attribute: Identifier;
  type: TypeUsage;
}

export interface Operation {
  expressionType: "Operation";
  left: Expression;
  op: "+" | "-" | "*" | "/";
  right: Expression;
}

export interface VariableUsage {
  expressionType: "VariableUsage";
  name: Identifier;
  type: TypeUsage;
}

export interface IfExpression {
  expressionType: "IfExpression";
  condition: Expression;
  block: Block;
  else: Block | null;
  type: TypeUsage;
}

export interface Expression {
  statementType: "Expression";
  subExpression:
    | LiteralInt
    | LiteralFloat
    | LiteralBool
    | LiteralString
    | LiteralStruct
    | FunctionCall
    | VariableUsage
    | IfExpression
    | StructGetter
    | Block
    | Operation;
  type: TypeUsage;
}

export interface ReturnStatement {
  statementType: "ReturnStatement";
  source: Expression;
}

export interface LetStatement {
  statementType: "LetStatement";
  variableName: Identifier;
  expression: Expression;
  type: TypeUsage;
}

export interface AssignmentStatement {
  statementType: "AssignmentStatement";
  source: VariableUsage | StructGetter;
  expression: Expression;
}

export type Statement =
  | ReturnStatement
  | LetStatement
  | AssignmentStatement
  | Expression;

export interface Block {
  statements: Statement[];
  type: TypeUsage;
}

export interface FunctionArgument {
  name: Identifier;
  type: TypeUsage;
}

export interface FunctionDeclaration {
  name: Identifier;
  arguments: FunctionArgument[];
  returnType: TypeUsage;
}

export interface Function {
  moduleItem: "Function";
  declaration: FunctionDeclaration;
  block: Block;
}

export interface StructTypeField {
  name: Identifier;
  type: TypeUsage;
}

export interface StructTypeDeclaration {
  moduleItem: "StructTypeDeclaration";
  typeDeclaration: "StructTypeDeclaration";
  name: Identifier;
  fields: StructTypeField[];
}

export interface TraitTypeDeclaration {
  moduleItem: "TraitTypeDeclaration";
  typeDeclaration: "TraitTypeDeclaration";
  name: Identifier;
  functions: FunctionDeclaration[];
}

export type TypeDeclaration = StructTypeDeclaration | TraitTypeDeclaration;

export interface Impl {
  moduleItem: "Impl";
  struct: NamedTypeUsage;
  trait: NamedTypeUsage | null;
  functions: Function[];
}

export type ModuleItem = Function | TypeDeclaration | Impl;

export interface Module {
  items: ModuleItem[];
}

export interface NamedTypeUsage {
  typeUsage: "NamedTypeUsage";
  name: Identifier;
}

export interface FunctionTypeUsage {
  typeUsage: "FunctionTypeUsage";
  arguments: TypeUsage[];
  returnType: TypeUsage;
}

export interface UnknownTypeUsage {
  typeUsage: "UnknownTypeUsage";
  name: string;
}

export type TypeUsage = NamedTypeUsage | FunctionTypeUsage | UnknownTypeUsage;

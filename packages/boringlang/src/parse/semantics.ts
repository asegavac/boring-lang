import {
  AssignmentStatement,
  Block,
  Expression,
  Function,
  FunctionArgument,
  FunctionCall,
  FunctionDeclaration,
  FunctionTypeUsage,
  Identifier,
  IfExpression,
  Impl,
  LetStatement,
  LiteralBool,
  LiteralFloat,
  LiteralInt,
  LiteralString,
  LiteralStruct,
  Module,
  ModuleItem,
  NamedTypeUsage,
  Operation,
  ReturnStatement,
  Statement,
  StructField,
  StructGetter,
  StructTypeDeclaration,
  StructTypeField,
  TraitTypeDeclaration,
  TypeDeclaration,
  TypeUsage,
  VariableUsage,
} from "./ast";
import { boringGrammar } from "./grammar";

let unknownTypeCounter = 0;
function nextUnknown() {
  let name = "S" + unknownTypeCounter.toString();
  unknownTypeCounter += 1;
  return name;
}

export const semantics = boringGrammar.createSemantics();
semantics.addOperation<any>("toAST", {
  LiteralInt(a): LiteralInt {
    console.log(this);
    console.log(a.source.startIdx);
    return {
      expressionType: "LiteralInt",
      value: this.sourceString,
      type: {
        typeUsage: "NamedTypeUsage",
        name: { name: "i64", spanStart: 0, spanEnd: 0 },
      },
    };
  },
  LiteralFloat(_1, _2, _3): LiteralFloat {
    return {
      expressionType: "LiteralFloat",
      value: this.sourceString,
      type: {
        typeUsage: "NamedTypeUsage",
        name: { name: "f64", spanStart: 0, spanEnd: 0 },
      },
    };
  },
  LiteralBool(_): LiteralBool {
    return {
      expressionType: "LiteralBool",
      value: this.sourceString,
      type: {
        typeUsage: "NamedTypeUsage",
        name: { name: "bool", spanStart: 0, spanEnd: 0 },
      },
    };
  },
  LiteralString(_1, text, _3): LiteralString {
    return {
      expressionType: "LiteralString",
      value: text.sourceString,
      type: {
        typeUsage: "NamedTypeUsage",
        name: { name: "String", spanStart: 0, spanEnd: 0 },
      },
    };
  },
  LiteralStructField(identifier, _2, expression): StructField {
    return {
      name: identifier.toAST(),
      expression: expression.toAST(),
    };
  },
  LiteralStruct(identifier, _2, fields, _4): LiteralStruct {
    return {
      expressionType: "LiteralStruct",
      name: identifier.toAST(),
      fields: fields.asIteration().children.map((c) => c.toAST()),
      type: { typeUsage: "NamedTypeUsage", name: identifier.toAST() },
    };
  },
  Identifier(_1, _2): Identifier {
    return {
      name: this.sourceString,
      spanStart: this.source.startIdx,
      spanEnd: this.source.endIdx,
    };
  },
  FunctionCall(expression, _2, args, _4): FunctionCall {
    return {
      expressionType: "FunctionCall",
      source: expression.toAST(),
      arguments: args.asIteration().children.map((c) => c.toAST()),
      type: { typeUsage: "UnknownTypeUsage", name: nextUnknown() },
    };
  },
  StructGetter(expression, _2, identifier): StructGetter {
    return {
      expressionType: "StructGetter",
      source: expression.toAST(),
      attribute: identifier.toAST(),
      type: { typeUsage: "UnknownTypeUsage", name: nextUnknown() },
    };
  },
  VariableUsage(identifier): VariableUsage {
    return {
      expressionType: "VariableUsage",
      name: identifier.toAST(),
      type: { typeUsage: "UnknownTypeUsage", name: nextUnknown() },
    };
  },
  IfExpression(_1, _2, expression, _4, block, _6, elseBlock): IfExpression {
    const eb = elseBlock.toAST();
    return {
      expressionType: "IfExpression",
      condition: expression.toAST(),
      block: block.toAST(),
      else: eb.length > 0 ? eb[0] : null,
      type: { typeUsage: "UnknownTypeUsage", name: nextUnknown() },
    };
  },
  Term(term): Expression {
    return term.toAST();
  },
  Term_parens(_1, term, _3): Expression {
    return term.toAST();
  },
  Factor(factor): Expression {
    return factor.toAST();
  },
  Expression(expression): Expression {
    return expression.toAST();
  },
  Expression_plus(expression, _2, factor): Operation {
    return {
      expressionType: "Operation",
      left: expression.toAST(),
      op: "+",
      right: factor.toAST(),
    };
  },
  Expression_minus(expression, _2, factor): Operation {
    return {
      expressionType: "Operation",
      left: expression.toAST(),
      op: "-",
      right: factor.toAST(),
    };
  },
  Factor_mult(factor, _2, term): Operation {
    return {
      expressionType: "Operation",
      left: factor.toAST(),
      op: "*",
      right: term.toAST(),
    };
  },
  Factor_div(factor, _2, term): Operation {
    return {
      expressionType: "Operation",
      left: factor.toAST(),
      op: "/",
      right: term.toAST(),
    };
  },
  Statement(statement): Statement {
    return statement.toAST();
  },
  ReturnStatement(_1, expression, _3): ReturnStatement {
    return {
      statementType: "ReturnStatement",
      source: expression.toAST(),
    };
  },
  LetStatement(_1, ident, _3, typeUsage, _5, expression, _7): LetStatement {
    const tu = typeUsage.toAST();
    return {
      statementType: "LetStatement",
      variableName: ident.toAST(),
      expression: expression.toAST(),
      type:
        tu.length > 0
          ? tu[0]
          : { typeUsage: "UnknownTypeUsage", name: nextUnknown() },
    };
  },
  AssignmentStatement(variable, _2, expression, _4): AssignmentStatement {
    return {
      statementType: "AssignmentStatement",
      source: variable.toAST(),
      expression: expression.toAST(),
    };
  },
  ExpressionStatement(expression, _2): Expression {
    return {
      statementType: "Expression",
      subExpression: expression.toAST(),
      type: { typeUsage: "UnknownTypeUsage", name: nextUnknown() },
    };
  },
  Block(_1, statements, expression, _4): Block {
    const lines = statements.asIteration().children.map((c) => c.toAST());
    const finalExpression = expression.toAST();
    lines.push(finalExpression.length > 0 ? finalExpression[0] : null);
    return {
      statements: lines,
      type: { typeUsage: "UnknownTypeUsage", name: nextUnknown() },
    };
  },
  NamedTypeUsage(name): NamedTypeUsage {
    return {
      typeUsage: "NamedTypeUsage",
      name: name.toAST(),
    };
  },
  TypeUsage_function_tu(_1, _2, args, _4, _5, returnType): FunctionTypeUsage {
    return {
      typeUsage: "FunctionTypeUsage",
      arguments: args.asIteration().children.map((c) => c.toAST()),
      returnType: returnType.toAST(),
    };
  },
  TypeUsage(typeUsage): TypeUsage {
    return typeUsage.toAST();
  },
  FunctionArgument(identifier, _2, typeUsage): FunctionArgument {
    return {
      name: identifier.toAST(),
      type: typeUsage.toAST(),
    };
  },
  FunctionDeclaration(
    _1,
    identifier,
    _3,
    args,
    _4,
    _5,
    returnType,
  ): FunctionDeclaration {
    return {
      name: identifier.toAST(),
      arguments: args.asIteration().children.map((c) => c.toAST()),
      returnType: returnType.toAST(),
    };
  },
  Function(declaration, block): Function {
    return {
      moduleItem: "Function",
      declaration: declaration.toAST(),
      block: block.toAST(),
    };
  },
  StructTypeField(identifier, _2, typeUsage): StructTypeField {
    return {
      name: identifier.toAST(),
      type: typeUsage.toAST(),
    };
  },
  StructTypeDeclaration(
    _1,
    identifier,
    _3,
    _4,
    fields,
    _6,
  ): StructTypeDeclaration {
    return {
      moduleItem: "StructTypeDeclaration",
      typeDeclaration: "StructTypeDeclaration",
      name: identifier.toAST(),
      fields: fields.asIteration().children.map((c) => c.toAST()),
    };
  },
  TraitMethod(declaration, _2): FunctionDeclaration {
    return declaration.toAST();
  },
  TraitTypeDeclaration(
    _1,
    identifier,
    _3,
    _4,
    methods,
    _5,
  ): TraitTypeDeclaration {
    return {
      moduleItem: "TraitTypeDeclaration",
      typeDeclaration: "TraitTypeDeclaration",
      name: identifier.toAST(),
      functions: methods.asIteration().children.map((c) => c.toAST()),
    };
  },
  TypeDeclaration(declaration): TypeDeclaration {
    return declaration.toAST();
  },
  Impl(_1, trait, _3, struct, _4, methods, _5): Impl {
    const tr = trait.toAST();
    return {
      moduleItem: "Impl",
      struct: struct.toAST(),
      trait: tr.length > 0 ? tr[0] : null,
      functions: methods.asIteration().children.map((c) => c.toAST()),
    };
  },
  ModuleItem(item): ModuleItem {
    return item.toAST();
  },
  Module(items): Module {
    return {
      items: items.asIteration().children.map((c) => c.toAST()),
    };
  },
  _iter(...children) {
    return children.map((c) => c.toAST());
  },
});

import * as ohm from "ohm-js";

export const boringGrammar = ohm.grammar(String.raw`
  Boringlang {
    ReturnStatement       = "return" Expression ";"
    LetStatement          = "let" Identifier (":" TypeUsage)? "=" Expression ";"
    AssignmentStatement   = VariableUsage "=" Expression ";"
                          | StructGetter "=" Expression ";"
    ExpressionStatement   = Expression ";"
    Statement             = ExpressionStatement
                          | LetStatement
                          | ReturnStatement
                          | AssignmentStatement
    LiteralInt            = digit+
    LiteralFloat          = digit* "." digit+
    LiteralBool           = "true" | "false"
    LiteralString         = "\"" (~"\"" any)* "\""
                          | "'" (~"'" any)* "'"
    LiteralStructField    = Identifier ":" Expression
    LiteralStruct         = Identifier "{" ListOf<LiteralStructField, ","> "}"
    Identifier            = (letter | "_")+ (letter | digit | "_")*
    FunctionCall          = Expression "(" ListOf<Expression, ","> ")"
    StructGetter          = Expression "." Identifier
    VariableUsage         = Identifier
    IfExpression          = "if" "(" Expression ")" Block ("else" Block)?
    Term                  = LiteralInt
                          | LiteralFloat
                          | LiteralBool
                          | LiteralString
                          | LiteralStruct
                          | IfExpression
                          | Block
                          | "(" Expression ")" -- parens
                          | VariableUsage
    Factor                = Factor "*" Term -- mult
                          | Factor "/" Term -- div
                          | Term
    Expression            = Expression "+" Factor -- plus
                          | Expression "-" Factor -- minus
                          | StructGetter
                          | FunctionCall
                          | Factor
    Block                 = "{" Statement* Expression? "}"
    NamedTypeUsage        = Identifier
    TypeUsage             = NamedTypeUsage
                          | "fn" "(" ListOf<TypeUsage, ","> ")" ":" TypeUsage -- function_tu
    FunctionArgument      = Identifier ":" TypeUsage
    FunctionDeclaration   = "fn" Identifier "(" ListOf<FunctionArgument, ","> ")" ":" TypeUsage
    Function              = FunctionDeclaration Block
    StructTypeField       = Identifier ":" TypeUsage
    StructTypeDeclaration = "type" Identifier "struct" "{" ListOf<StructTypeField, ","> "}"
    TraitMethod           = FunctionDeclaration ";"
    TraitTypeDeclaration  = "type" Identifier "trait" "{" TraitMethod* "}"
    TypeDeclaration       = StructTypeDeclaration
                          | TraitTypeDeclaration
    Impl                  = "impl" (NamedTypeUsage "for")? NamedTypeUsage "{" Function* "}"
    ModuleItem            = Function
                          | TypeDeclaration
                          | Impl
    Module                = ModuleItem*

    lineTerminator        = "\n" | "\r" | "\u2028" | "\u2029"
    comment               = "//" (~lineTerminator any)* lineTerminator
    space                += comment
  }
`);

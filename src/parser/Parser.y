{
module Parser where
import Lexer
import Ast
}

%name parseRepl ReplInput
%name parseProgram Program
%tokentype { Token }
%error { parseError }

%token
  if            { KwIf _ }
  else          { KwElse _ }
  true          { KwTrue _ }
  false         { KwFalse _ }
  for           { KwFor _ }
  continue      { KwContinue _ }
  break         { KwBreak _ }
  fn            { KwFn _ }
  struct        { KwStruct _ }
  enum          { KwEnum _ }
  return        { KwReturn _ }
  int           { KwInt _ }
  float         { KwFloat _ }
  bool          { KwBool _ }
  '='           { Equal _ }
  '<'           { Lt _ }
  '>'           { Gt _ }
  '=='          { CmpEq _ }
  '!='          { CmpNeq _ }
  '<='          { CmpLeq _ }
  '>='          { CmpGeq _ }
  '->'          { ArrowRight _ }
  and           { KwAnd _ }
  or            { KwOr _ }
  '!'           { KwNot _ }
  '+'           { Plus _ }
  '-'           { Minus _ }
  '>>'          { ShiftRight _ }
  '<<'          { ShiftLeft _ }
  '*'           { Times _ }
  '/'           { Div _ }
  '&'           { Ampersand _ }
  '('           { OpenPar _ }
  ')'           { ClosePar _ }
  '{'           { OpenBraces _ }
  '}'           { CloseBraces _ }
  '['           { OpenBrack _ }
  ']'           { CloseBrack _ }
  '::'          { DoubleDot _ }
  '.'           { Dot _ }
  ','           { Comma _ }
  ';'           { Semicolon _ }
  ':'           { Colon _ }
  "'"           { SingleQuote _ }
  '"'           { DoubleQuote _ }
  ID            { Id _ _ }
  INT_LITERAL   { IntLiteral _ _ }
  FLOAT_LITERAL { FloatLiteral _ _ }

%%

Program : TopLevels { Program $1 }

TopLevels : TopLevel TopLevels { $1 : $2 }
          |                    { [] }

TopLevel : Decl { TopLevelDecl $1 }

ReplInput : Decl { ReplDecl $1 }
          | Expr { ReplExpr $1 }

Decl : ValueDecl ';' { $1 }

ValueDecl : ID ':' Type               { ValueDecl Mutable (toIdent $1) (Just $3) Nothing }
          | ID ':' OptType '=' Expr   { ValueDecl Mutable (toIdent $1) $3 (Just $5) }
          | ID ':' OptType ':' Expr   { ValueDecl Constant (toIdent $1) $3 (Just $5) }

OptType : Type { Just $1 }
        |      { Nothing }

Type : ID       { TypeName (toIdent $1) }
     | TypeLit  { $1 }

TypeLit : bool  { BoolType (token_posn $1) }
        | int   { IntType (token_posn $1) }
        | float { FloatType (token_posn $1) }

Expr : AddExpr { $1 }

AddExpr : AddExpr '+' MulExpr { BinaryExpr (token_posn $2) AddOp $1 $3 }
        | MulExpr             { $1 }

MulExpr : MulExpr '*' AtomExpr { BinaryExpr (token_posn $2) MulOp $1 $3 }
        | MulExpr '/' AtomExpr { BinaryExpr (token_posn $2) DivOp $1 $3 }
        | AtomExpr             { $1 }

AtomExpr : INT_LITERAL   { toIntLit $1 }
         | FLOAT_LITERAL { toFloatLit $1 }
         | true          { BoolLit (token_posn $1) True }
         | false         { BoolLit (token_posn $1) False }
         | '(' Expr ')'  { $2 }

{
parseError :: [Token] -> a
parseError _ = error "parse error"

toIdent :: Token -> Ident
toIdent (Id pos name) = Ident pos name
toIdent _ = error "internal parser error: expected identifier"

toIntLit :: Token -> Expr
toIntLit (IntLiteral pos value) = IntLit pos value
toIntLit _ = error "internal parser error: expected integer literal"

toFloatLit :: Token -> Expr
toFloatLit (FloatLiteral pos value) = FloatLit pos value
toFloatLit _ = error "internal parser error: expected float literal"
}

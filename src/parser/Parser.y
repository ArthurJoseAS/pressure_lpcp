{
module Parser where
import Lexer
import Ast 
}

%name parseRepl ReplInput
%name parseProgram Program
%tokentype { Token }
%error { parseError }
%monad { Alex }
%lexer { lexer } { TokenEOF }

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
  uint          { KwUint _ }
  float         { KwFloat _ }
  bool          { KwBool _ }
  i8            { KwI8 _ }
  i16           { KwI16 _ }
  i32           { KwI32 _ }
  i64           { KwI64 _ }
  u8            { KwU8 _ }
  u16           { KwU16 _ }
  u32           { KwU32 _ }
  u64           { KwU64 _ }
  f32           { KwF32 _ }
  f64           { KwF64 _ }
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

TopLevel : Expr ';' { TopLevelStmt $1 }

ReplInput : Expr ';' { ReplStmt $1 }
          | Expr { ReplExpr $1 }

ValueDecl : ID ':' Type               { ValueDecl Mutable (toIdent $1) (Just $3) Nothing }
          | ID ':' OptType '=' Expr   { ValueDecl Mutable (toIdent $1) $3 (Just $5) }
          | ID ':' OptType ':' Expr   { ValueDecl Constant (toIdent $1) $3 (Just $5) }

OptType : Type { Just $1 }
        |      { Nothing }

Type : ID       { TypeName (toIdent $1) }
     | TypeLit  { $1 }

TypeLit : bool  { BoolType (token_posn $1) }
        | int   { IntType (token_posn $1) Signed I32 }
        | uint  { IntType (token_posn $1) Unsigned I32 }
        | float { FloatType (token_posn $1) F64 }
        | i8    { IntType (token_posn $1) Signed I8 }
        | i16   { IntType (token_posn $1) Signed I16 }
        | i32   { IntType (token_posn $1) Signed I32 }
        | i64   { IntType (token_posn $1) Signed I64 }
        | u8    { IntType (token_posn $1) Unsigned I8 }
        | u16   { IntType (token_posn $1) Unsigned I16 }
        | u32   { IntType (token_posn $1) Unsigned I32 }
        | u64   { IntType (token_posn $1) Unsigned I64 }
        | f32   { FloatType (token_posn $1) F32 }
        | f64   { FloatType (token_posn $1) F64 }

{- expressions -}

Expr : ValueDecl { DeclExpr (declPos $1) $1 }
     | LogicalOrExpr { $1 }

LogicalOrExpr : LogicalOrExpr or LogicalAndExpr { BinaryExpr (token_posn $2) OrOp $1 $3 }
              | LogicalAndExpr                  { $1 }

LogicalAndExpr : LogicalAndExpr and ComparisonExpr { BinaryExpr (token_posn $2) AndOp $1 $3 }
               | ComparisonExpr                    { $1 }

ComparisonExpr : AddExpr CompareOp AddExpr { let (pos, op) = $2 in BinaryExpr pos op $1 $3 }
               | AddExpr                   { $1 }

CompareOp : '==' { (token_posn $1, EqOp) }
          | '!=' { (token_posn $1, NeqOp) }
          | '<'  { (token_posn $1, LtOp) }
          | '<=' { (token_posn $1, LeqOp) }
          | '>'  { (token_posn $1, GtOp) }
          | '>=' { (token_posn $1, GeqOp) }

AddExpr : AddExpr '+' MulExpr { BinaryExpr (token_posn $2) AddOp $1 $3 }
        | AddExpr '-' MulExpr { BinaryExpr (token_posn $2) SubOp $1 $3 }
        | MulExpr             { $1 }

MulExpr : MulExpr '*' AtomExpr { BinaryExpr (token_posn $2) MulOp $1 $3 }
        | MulExpr '/' AtomExpr { BinaryExpr (token_posn $2) DivOp $1 $3 }
        | AtomExpr             { $1 }

AtomExpr : INT_LITERAL   { toIntLit $1 }
         | FLOAT_LITERAL { toFloatLit $1 }
         | true          { BoolLit (token_posn $1) True }
         | false         { BoolLit (token_posn $1) False }
         | '(' Expr ')'  { $2 }
         | ID            { VarExpr (token_posn $1) (toIdent $1) }

{
parseError :: Token -> Alex a
parseError tok =
  let AlexPn _ line col = token_posn tok
  in alexError $ "at " ++ show line ++ ":" ++ show col
              ++ ": unexpected " ++ show tok

toIdent :: Token -> Ident
toIdent (Id pos name) = Ident pos name
toIdent _ = error "internal parser error: expected identifier"

toIntLit :: Token -> Expr
toIntLit (IntLiteral pos value) = IntLit pos value
toIntLit _ = error "internal parser error: expected integer literal"

toFloatLit :: Token -> Expr
toFloatLit (FloatLiteral pos value) = FloatLit pos value
toFloatLit _ = error "internal parser error: expected float literal"

declPos :: Decl -> AlexPosn
declPos (ValueDecl _ (Ident pos _) _ _) = pos

genAst = runAlex
}

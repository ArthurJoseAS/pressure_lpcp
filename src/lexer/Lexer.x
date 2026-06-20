{
module Lexer
  ( Token(..)
  , Alex(..)
  , AlexPosn(..)
  , alexError
  , alexMonadScan
  , lexer
  , runAlex
  , tokenize
  , tokenizeEither
  , token_posn
  ) where
}

%wrapper "monad"

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters

tokens :-

  $white+                         ;
  "//".*                          ;
  if                                 { mkToken KwIf }
  else                               { mkToken KwElse }
  true                               { mkToken KwTrue }
  false                              { mkToken KwFalse }
  for                                { mkToken KwFor }
  continue                           { mkToken KwContinue }
  break                              { mkToken KwBreak }
  fn                                 { mkToken KwFn }
  struct                             { mkToken KwStruct }
  enum                               { mkToken KwEnum }
  return                             { mkToken KwReturn }
  int                                { mkToken KwInt }
  float                              { mkToken KwFloat }
  bool                               { mkToken KwBool }
  \=                                 { mkToken Equal }
  \<                                 { mkToken Lt }
  \>                                 { mkToken Gt }
  \=\=                               { mkToken CmpEq }
  \!\=                               { mkToken CmpNeq }
  \<\=                               { mkToken CmpLeq }
  \>\=                               { mkToken CmpGeq }
  \-\>                               { mkToken ArrowRight }
  and                                { mkToken KwAnd }
  or                                 { mkToken KwOr }
  !                                  { mkToken KwNot }
  \&                                 { mkToken Ampersand }
  \+                                 { mkToken Plus }
  \-                                 { mkToken Minus }
  \>\>                               { mkToken ShiftRight }
  \<\<                               { mkToken ShiftLeft }
  \*                                 { mkToken Times }
  \/                                 { mkToken Div }
  \(                                 { mkToken OpenPar }
  \)                                 { mkToken ClosePar }
  \{                                 { mkToken OpenBraces }
  \}                                 { mkToken CloseBraces }
  \[                                 { mkToken OpenBrack }
  \]                                 { mkToken CloseBrack }
  \.\.                               { mkToken DoubleDot }
  \.                                 { mkToken Dot }
  \,                                 { mkToken Comma }
  \;                                 { mkToken Semicolon }
  \:                                 { mkToken Colon }
  \'                                 { mkToken SingleQuote }
  \"                                 { mkToken DoubleQuote }
  [$alpha \_] [$alpha $digit \_ \']* { mkTokenText Id }
  $digit+ \. $digit+                 { mkTokenText (\p s -> FloatLiteral p (read s)) }
  $digit+                            { mkTokenText (\p s -> IntLiteral p (read s)) }

{

data Token
  = TokenEOF
  | KwIf AlexPosn
  | KwElse AlexPosn
  | KwTrue AlexPosn
  | KwFalse AlexPosn
  | KwFor AlexPosn
  | KwContinue AlexPosn
  | KwBreak AlexPosn
  | KwFn AlexPosn
  | KwStruct AlexPosn
  | KwEnum AlexPosn
  | KwReturn AlexPosn
  | KwInt AlexPosn
  | KwFloat AlexPosn
  | KwBool AlexPosn
  | Equal AlexPosn
  | Lt AlexPosn
  | Gt AlexPosn
  | CmpEq AlexPosn
  | CmpNeq AlexPosn
  | CmpLeq AlexPosn
  | CmpGeq AlexPosn
  | ArrowRight AlexPosn
  | KwAnd AlexPosn
  | KwOr AlexPosn
  | KwNot AlexPosn
  | Plus AlexPosn
  | Minus AlexPosn
  | ShiftRight AlexPosn
  | ShiftLeft AlexPosn
  | Times AlexPosn
  | Div AlexPosn
  | Ampersand AlexPosn
  | OpenPar AlexPosn
  | ClosePar AlexPosn
  | OpenBraces AlexPosn
  | CloseBraces AlexPosn
  | OpenBrack AlexPosn
  | CloseBrack AlexPosn
  | DoubleDot AlexPosn
  | Dot AlexPosn
  | Comma AlexPosn
  | Semicolon AlexPosn
  | Colon AlexPosn
  | SingleQuote AlexPosn
  | DoubleQuote AlexPosn
  | Id AlexPosn String
  | IntLiteral AlexPosn Integer
  | FloatLiteral AlexPosn Double
  deriving (Show, Eq)

token_posn :: Token -> AlexPosn
token_posn (KwIf p)       = p
token_posn (KwElse p)     = p
token_posn (KwTrue p)     = p
token_posn (KwFalse p)    = p
token_posn (KwFor p)      = p
token_posn (KwContinue p) = p
token_posn (KwBreak p)    = p
token_posn (KwFn p)       = p
token_posn (KwStruct p)   = p
token_posn (KwEnum p)     = p
token_posn (KwReturn p)   = p
token_posn (KwInt p)   = p
token_posn (KwFloat p)   = p
token_posn (KwBool p)   = p
token_posn (Equal p)      = p
token_posn (Lt p)         = p
token_posn (Gt p)         = p
token_posn (CmpEq p)      = p
token_posn (CmpNeq p)     = p
token_posn (CmpLeq p)     = p
token_posn (CmpGeq p)     = p
token_posn (ArrowRight p) = p
token_posn (KwAnd p)      = p
token_posn (KwOr p)       = p
token_posn (KwNot p)    = p
token_posn (Plus p)       = p
token_posn (Minus p)      = p
token_posn (ShiftRight p) = p
token_posn (ShiftLeft p)  = p
token_posn (Times p)      = p
token_posn (Div p)        = p
token_posn (Ampersand p)     = p
token_posn (OpenPar p)    = p
token_posn (ClosePar p)   = p
token_posn (OpenBraces p) = p
token_posn (CloseBraces p)= p
token_posn (OpenBrack p)  = p
token_posn (CloseBrack p) = p
token_posn (DoubleDot p)  = p
token_posn (Dot p)        = p
token_posn (Comma p)      = p
token_posn (Semicolon p)  = p
token_posn (Colon p)      = p
token_posn (SingleQuote p)= p
token_posn (DoubleQuote p)= p
token_posn (Id p _)       = p
token_posn (IntLiteral p _)   = p
token_posn (FloatLiteral p _) = p
token_posn TokenEOF       = alexStartPos

mkToken :: (AlexPosn -> Token) -> AlexInput -> Int -> Alex Token
mkToken makeToken (pos, _, _, _) _ = return (makeToken pos)

mkTokenText :: (AlexPosn -> String -> Token) -> AlexInput -> Int -> Alex Token
mkTokenText makeToken (pos, _, _, input) len = return (makeToken pos (take len input))

alexEOF :: Alex Token
alexEOF = return TokenEOF

tokenizeEither :: String -> Either String [Token]
tokenizeEither input = runAlex input go
  where
    go = do
      token' <- alexMonadScan
      case token' of
        TokenEOF -> return []
        _ -> do
          tokens' <- go
          return (token' : tokens')

tokenize :: String -> [Token]
tokenize input =
  case tokenizeEither input of
    Left err -> error err
    Right tokens -> tokens

lexer :: (Token -> Alex a) -> Alex a
lexer cont = alexMonadScan >>= cont
}

{- HLINT ignore "Eta reduce" -}
{- HLINT ignore "Use lambda-case" -}
module ParserHelpers.Terminals where
import Lexer
import Text.Parsec
import Text.Parsec.Pos

--função para comparar tokens que o gemini fez
matchToken :: (Token -> Maybe a) -> Parsec [Token] st a
matchToken testFunc = tokenPrim show updatePosition testFunc
  where
    updatePosition :: SourcePos -> Token -> [Token] -> SourcePos
    updatePosition currentPos currentToken remainingTokens =
      case remainingTokens of
        []          -> currentPos
        (nextTok:_) -> let AlexPn _ line col = token_posn nextTok
                       in newPos "input" line col

letP       = matchToken (\t -> case t of { TokenLet _ -> Just (); _ -> Nothing })
colonP     = matchToken (\t -> case t of { Colon _    -> Just (); _ -> Nothing })
equalP     = matchToken (\t -> case t of { Equal _    -> Just (); _ -> Nothing })
semicolonP = matchToken (\t -> case t of { Semicolon _-> Just (); _ -> Nothing })

idP        = matchToken (\t -> case t of { Id _ name      -> Just name; _ -> Nothing })
intLitP    = matchToken (\t -> case t of
    IntLiteral _ val -> Just val
    _                -> Nothing)

fnP = matchToken (\t -> case t of
  TokenFn _ -> Just ()
  _ -> Nothing)

openParP = matchToken (\t -> case t of
  OpenPar _ -> Just ()
  _ -> Nothing)

closeParP = matchToken (\t -> case t of
  ClosePar _ -> Just ()
  _ -> Nothing)

mutP = matchToken (\t -> case t of
  TokenMut _ -> Just ()
  _ -> Nothing)

commaP = matchToken (\t -> case t of
  Comma _ -> Just ()
  _ -> Nothing)

ifP = matchToken (\t -> case t of
  TokenIf _ -> Just ()
  _ -> Nothing)
elseP = matchToken (\t -> case t of
  TokenElse _ -> Just ()
  _ -> Nothing)

whileP = matchToken (\t -> case t of
  TokenWhile _ -> Just ()
  _ -> Nothing)

returnP = matchToken (\t -> case t of
  TokenReturn _ -> Just ()
  _ -> Nothing)

andP = matchToken (\t -> case t of
  TokenAnd _ -> Just ()
  _ -> Nothing)
orP = matchToken (\t -> case t of
  TokenOr _ -> Just ()
  _ -> Nothing)
notP = matchToken (\t -> case t of
  TokenNot _ -> Just ()
  _ -> Nothing)
  
compSymbP = matchToken (\t -> case t of TokenCompSymb _ symb -> Just symb; _ -> Nothing)
openBraceP = matchToken (\t -> case t of OpenBraces _ -> Just ();_ -> Nothing)

closeBraceP = matchToken (\t -> case t of CloseBraces _ -> Just (); _ -> Nothing)

matchP = matchToken (\t -> case t of { TokenMatch _ -> Just (); _ -> Nothing })

trueP = matchToken (\t -> case t of { TokenTrue _ -> Just (); _ -> Nothing })

falseP = matchToken (\t -> case t of { TokenFalse _ -> Just (); _ -> Nothing })

forP = matchToken (\t -> case t of { TokenFor _ -> Just (); _ -> Nothing })

continueP = matchToken (\t -> case t of { TokenContinue _ -> Just (); _ -> Nothing })

breakP = matchToken (\t -> case t of { TokenBreak _ -> Just (); _ -> Nothing })

structP = matchToken (\t -> case t of { TokenStruct _ -> Just (); _ -> Nothing })

pubP = matchToken (\t -> case t of { TokenPub _ -> Just (); _ -> Nothing })

enumP = matchToken (\t -> case t of { TokenEnum _ -> Just (); _ -> Nothing })

isP = matchToken (\t -> case t of { TokenIs _ -> Just (); _ -> Nothing })

plusP = matchToken (\t -> case t of { Plus _ -> Just (); _ -> Nothing })

minusP = matchToken (\t -> case t of { Minus _ -> Just (); _ -> Nothing })

timesP = matchToken (\t -> case t of { Times _ -> Just (); _ -> Nothing })

divP = matchToken (\t -> case t of { Div _ -> Just (); _ -> Nothing })

bitAndP = matchToken (\t -> case t of { BitAnd _ -> Just (); _ -> Nothing })

bitOrP = matchToken (\t -> case t of { BitOr _ -> Just (); _ -> Nothing })

bitNotP = matchToken (\t -> case t of { BitNot _ -> Just (); _ -> Nothing })

shiftRightP = matchToken (\t -> case t of { ShiftRight _ -> Just (); _ -> Nothing })

shiftLeftP = matchToken (\t -> case t of { ShiftLeft _ -> Just (); _ -> Nothing })

openBrackP = matchToken (\t -> case t of { OpenBrack _ -> Just (); _ -> Nothing })

closeBrackP = matchToken (\t -> case t of { CloseBrack _ -> Just (); _ -> Nothing })

dotP = matchToken (\t -> case t of { Dot _ -> Just (); _ -> Nothing })

doubleDotP = matchToken (\t -> case t of { DoubleDot _ -> Just (); _ -> Nothing })

singleQuoteP = matchToken (\t -> case t of { SingleQuote _ -> Just (); _ -> Nothing })

doubleQuoteP = matchToken (\t -> case t of { DoubleQuote _ -> Just (); _ -> Nothing })

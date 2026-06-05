{- HLINT ignore "Use newtype instead of data" -}
{- HLINT ignore "Eta reduce" -}
module Parser (parser) where
import Lexer (Token(..), AlexPosn(..))
import Text.Parsec
import ParserHelpers.Terminals



data VarDecl = VarDecl{
        varId :: String,
        varType :: Maybe String,
        varInitVal :: Maybe Int
} deriving (Show, Eq)
data ParamDecl = ParamDecl{
    paramType :: String,
    paramName :: String
} deriving (Show, Eq)

data FnDecl = FnDecl{
    fnName :: String,
    fnParamList :: [ParamDecl],
    fnReturn :: String,
    fnBody :: [Token]
} deriving (Show, Eq)

data FnMainDecl = FnMainDecl{
    mainStatements :: [Token]
} deriving (Show, Eq)

data GlobalDeclType 
    = GlobalVar VarDecl
    | GlobalFn FnDecl
    | MainFn 
    deriving (Show, Eq)

parseGlobalDecl :: Parsec [Token] st [GlobalDeclType]
parseGlobalDecl = do
    global_declarations <- many (
        try parseGlobalVar
        -- try parseGlobalFn <|> parseFnMain
         )
    eof
    return global_declarations

parseGlobalVar :: Parsec [Token] st GlobalDeclType
parseGlobalVar = do 
    _ <- letP 
    name <- idP
    vartype <- optionMaybe (do 
            _ <- colonP
            idP
        )
    initval <- optionMaybe (do
                _ <- equalP
                intLitP
            )
    _ <- semicolonP

    return  (GlobalVar (VarDecl name vartype initval))


-- invocação do parser para o símbolo de partida 

parser :: [Token] -> Either ParseError [GlobalDeclType]
parser tokens = runParser parseGlobalDecl () "Error message" tokens


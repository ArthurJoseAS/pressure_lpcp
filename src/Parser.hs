{- HLINT ignore "Use newtype instead of data" -}
{- HLINT ignore "Eta reduce" -}
module Parser (parser) where
import Lexer (Token(..), AlexPosn(..))
import Text.Parsec
import ParserHelpers.Terminals
import Data.Maybe
import GHC.IO.Handle.Types (Handle__(Handle__))


data VarDecl = VarDecl{
        varId :: String,
        varType :: Maybe String,
        varInitVal :: Maybe Int
} deriving (Show, Eq)
data ParamDecl = ParamDecl{
    paramMut :: Bool,
    paramType :: String,
    paramName :: String
} deriving (Show, Eq)

data FnDecl = FnDecl{
    fnName :: String,
    fnParamList :: [ParamDecl],
    fnReturn :: String,
    fnBody :: [StmtDecl]
} deriving (Show, Eq)

data FnMainDecl = FnMainDecl{
    mainStatements :: [StmtDecl]
} deriving (Show, Eq)

data StmtDecl
    = VarStmt VarDecl
    -- | ExpStmt ExpDecl
    -- | IfElseStmt IfElseDecl
    -- | ReptStmt ReptDecl
    -- | RetStmt RetDecl
    deriving (Show, Eq)


data GlobalDeclType
    = GlobalVar VarDecl
    | GlobalFn FnDecl
    | MainFn FnMainDecl
    deriving (Show, Eq)

parseGlobalDecl :: Parsec [Token] st [GlobalDeclType]
parseGlobalDecl = do
    global_declarations <- many (
        try parseGlobalVar <|>
        try parseGlobalFn 
        <|> parseFnMain
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

parseGlobalFn :: Parsec [Token] st GlobalDeclType
parseGlobalFn = do
    fnName <- fnP >> idP
    if fnName == "main" 
        then fail "Invalid main definition"
    else do 
        paramList <- openParP  >> parseParamList
        retType <- closeParP
                >> ((openBraceP >> return "void")
                    <|> (do {t <- idP; openBraceP; return t}))

        stmtList <- parseStmtList
        _ <- closeBraceP
        return (GlobalFn (FnDecl fnName paramList retType stmtList))

parseParamList :: Parsec [Token] st [ParamDecl]
parseParamList = parseParam `sepEndBy` commaP

parseParam :: Parsec [Token] st ParamDecl
parseParam = do
    maybeMut <- optionMaybe mutP
    let isMut = case maybeMut of
                  Just _  -> True
                  Nothing -> False

    name <- idP
    _    <- colonP
    ParamDecl isMut name <$> idP

parseStmtList :: Parsec [Token] st [StmtDecl]
parseStmtList = parseStmt `sepEndBy` semicolonP

parseStmt :: Parsec [Token] st StmtDecl
parseStmt = do
    VarStmt <$> parseLocalVar

parseLocalVar :: Parsec [Token] st VarDecl
parseLocalVar = do
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

    return (VarDecl name vartype initval)

parseFnMain :: Parsec [Token] st GlobalDeclType
parseFnMain = do
    _ <- fnP 
    mainId <- idP
    if mainId == "main" then
        do
            _ <- openParP >> closeParP >> openBraceP
            stmtlist <- parseStmtList
            _ <- closeBraceP
            return (MainFn (FnMainDecl stmtlist))
    else fail "Could not find main"

-- invocação do parser para o símbolo de partida 
parser :: [Token] -> Either ParseError [GlobalDeclType]
parser tokens = runParser parseGlobalDecl () "Error message" tokens


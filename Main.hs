module Main where
  import Data.Functor.Identity
  import Data.List (nub)
  import System.Process
  import Text.Parsec (Parsec, ParsecT, modifyState)
  import Text.ParserCombinators.Parsec
  import BitsyGen
  import BitsyTypes

  newlines :: ParsecT [Char] ParserState Identity ()
  newlines = skipMany newline

  number :: ParsecT [Char] ParserState Identity BitsyExpr
  number = do
    ds <- many1 digit
    return $ BInt (read ds)

  word :: ParsecT [Char] ParserState Identity String
  word = many1 (oneOf (['a'..'z'] ++ ['A'..'Z']))

  parseAssign :: ParsecT [Char] ParserState Identity BitsyExpr
  parseAssign = do
    spaces
    vn <- parseVar
    modifyState (addDecVar vn)
    spaces
    string "="
    spaces
    expr <- (try parseExpr) <|> (try parseLiteral) <|> (try parseVar)
    return $ BAssign vn expr

  parseRead :: ParsecT [Char] ParserState Identity BitsyExpr
  parseRead = do
    spaces
    string "READ"
    spaces
    v <- word
    return $ BRead (BVarName v)

  parseLiteral :: ParsecT [Char] ParserState Identity BitsyExpr
  parseLiteral = do
    n <- number
    return n

  parseVar :: ParsecT [Char] ParserState Identity BitsyExpr
  parseVar = do
    w <- word
    modifyState (addUsedVar (BVarName w))
    return $ BVarName w

  parsePrint :: ParsecT [Char] ParserState Identity BitsyExpr
  parsePrint = do
    spaces
    string "PRINT"
    spaces
    tp <- (try parseExpr) <|> (try parseLiteral) <|> (try parseVar)
    newline
    return $ BPrint tp

  parseBreak :: ParsecT [Char] ParserState Identity BitsyExpr
  parseBreak = spaces >> string "BREAK" >> newline >> return BBreak

  parseLoop :: ParsecT [Char] ParserState Identity BitsyExpr
  parseLoop = do
    spaces
    string "LOOP"
    newline
    b <- parseBlock
    newlines
    spaces
    string "END"
    return $ BLoop b

  parseExpr :: ParsecT [Char] ParserState Identity BitsyExpr
  parseExpr = do
    spaces
    num1 <- (try number) <|> (try parseVar)
    spaces
    op <- oneOf ['+', '-', '*', '/', '%']
    spaces
    num2 <- (try parseExpr) <|> (try number) <|> (try parseVar)
    let op' = case op of
                '+' -> BAdd "+"
                '-' -> BSub "-"
                '*' -> BMul "*"
                '/' -> BDiv "/"
                '%' -> BMod "%"
    return $ BExpression num1 op' num2

  parseIf :: ParsecT [Char] ParserState Identity BitsyExpr
  parseIf = do
    spaces
    t <- (try (string "IFP")) <|> (try (string "IFZ")) <|> (try (string "IFN"))
    spaces
    expr <- (try parseExpr) <|> (try parseLiteral) <|> (try parseVar)
    newline
    b <- parseBlock
    spaces
    b1 <- optionMaybe (try parseElse)
    spaces
    string "END"
    return $ BIf t expr b (case b1 of { (Just s) -> s; Nothing -> (BElse BVoid) })

  parseElse :: ParsecT [Char] ParserState Identity BitsyExpr
  parseElse = do
    string "ELSE"
    newline
    b1 <- parseBlock
    return $ BElse b1

  parseComment :: ParsecT [Char] ParserState Identity BitsyExpr
  parseComment = do
    conts <- between (char '{') (char '}') (many (noneOf ['}']))
    return $ BComment conts

  parseBlock :: ParsecT [Char] ParserState Identity BitsyExpr
  parseBlock = do
    exprs <- many1 $ (try parseIf) <|> (try parseLoop) <|> (try parseRead) <|> (try parsePrint) <|> (try parseAssign) <|> (try parseBreak) <|> (try parseExpr)
    return $ BBlock exprs

  parseProgram :: Parsec [Char] ParserState (BitsyExpr, ParserState)
  parseProgram = do
    comm <- parseComment
    newline
    string "BEGIN"
    newline
    mainBlock <- parseBlock
    newlines
    string "END"
    n <- getState
    let varlist = BVarList $ map (\s -> BDecVar s) (nub (declaredVars n))
    return $ (BProgram comm mainBlock varlist, n)

  main :: IO ()
  main = do
    input <- readFile "main.bitsy"
    let (parseResult, endState) = case runParser parseProgram (ParserState [] [] False []) "ERROR" input of
                  Right (s, n) -> (s, n)
                  Left e -> (BError e, ParserState [] [] True [])
    if (not (isError endState)) then (writeFile "main.c" (compile parseResult) >> putStrLn "Parsing Complete" >> runCommand "gcc -o main main.c && rm main.c" >> putStrLn "Compilation complete, created \"main\"") else (mapM_ putStrLn (errors endState) >> putStrLn (show parseResult))
    
module Main where
  import Text.ParserCombinators.Parsec hiding (spaces)
  
  data BitsyExpr = BProgram [BitsyExpr] BitsyExpr -- COMMENTS BEGIN block END
                 | BBlock [BitsyExpr] -- [if | loop | break | print | read | assign]
                 | BIf String BitsyExpr [BitsyExpr] -- keyword (IFZ | IFP | IFN) expr block else-block ended by END
                 | BLoop BitsyExpr -- LOOP <block> END
                 | BBreak -- BREAK
                 | BPrint BitsyExpr -- expr
                 | BRead BitsyExpr -- var-name
                 | BVarName String
                 | BAssign BitsyExpr BitsyExpr -- var-name expr
                 | BAdd String
                 | BSub String
                 | BMul String
                 | BDiv String
                 | BInt Int
                 | BExpression BitsyExpr BitsyExpr BitsyExpr -- var1 operator var2
                 | BComment String
                 deriving Show

  spaces :: Parser ()
  spaces = skipMany space

  newlines :: Parser ()
  newlines = skipMany newline

  number :: Parser BitsyExpr
  number = do
    ds <- many1 digit
    return $ BInt (read ds)

  word :: Parser String
  word = many1 (oneOf (['a'..'z'] ++ ['A'..'Z']))

  parseAssign :: Parser BitsyExpr
  parseAssign = do
    spaces
    vn <- parseVar
    spaces
    string "="
    spaces
    expr <- choice [parseLiteral, parseExpr]
    return $ BAssign vn expr

  parseRead :: Parser BitsyExpr
  parseRead = do
    spaces
    string "READ"
    spaces
    v <- word
    return $ BRead (BVarName v)

  parseLiteral :: Parser BitsyExpr
  parseLiteral = do
    n <- number
    return n

  parseVar :: Parser BitsyExpr
  parseVar = do
    w <- word
    return $ BVarName w

  parsePrint :: Parser BitsyExpr
  parsePrint = do
    spaces
    string "PRINT"
    spaces
    tp <- (try parseExpr) <|> (try parseVar) <|> (try parseLiteral)
    return $ BPrint tp

  parseBreak :: Parser BitsyExpr
  parseBreak = spaces >> string "BREAK" >> newline >> return BBreak

  parseLoop :: Parser BitsyExpr
  parseLoop = do
    spaces
    string "LOOP"
    newline
    b <- parseBlock
    spaces
    string "END"
    newline
    return $ BLoop b

  parseExpr :: Parser BitsyExpr
  parseExpr = do
    num1 <- number
    spaces
    op <- oneOf ['+', '-', '*', '/']
    spaces
    num2 <- number
    let op' = case op of
                '+' -> BAdd "+"
                '-' -> BSub "-"
                '*' -> BMul "*"
                '/' -> BDiv "/"
    return $ BExpression num1 op' num2

  parseIf :: Parser BitsyExpr
  parseIf = do
    spaces
    t <- word
    spaces
    expr <- parseExpr
    newline
    b <- parseBlock
    spaces
    string "END"
    newline
    return $ BIf t expr [b]

  parseComment :: Parser BitsyExpr
  parseComment = do
    conts <- between (char '{') (char '}') (many (noneOf ['}']))
    return $ BComment conts

  parseBlock :: Parser BitsyExpr
  parseBlock = do
    exprs <- many1 $ (try parseIf) <|> (try parseLoop) <|> (try parseRead) <|> (try parsePrint) <|> (try parseAssign) <|> (try parseBreak)
    return $ BBlock exprs

  parseProgram :: Parser BitsyExpr
  parseProgram = do
    comm <- parseComment
    newline
    string "BEGIN"
    newline
    mainBlock <- parseBlock
    string "END"
    return $ BProgram [mainBlock] comm

  main :: IO ()
  main = do
    input <- readFile "test.bitsy"
    case parse parseProgram "error!" input of
      Right s -> putStrLn (show s)
      Left e -> putStrLn (show e)
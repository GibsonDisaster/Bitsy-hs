
module Main where
  import Text.ParserCombinators.Parsec

  data BitsyExpr = BProgram BitsyExpr BitsyExpr -- COMMENTS BEGIN block END
                 | BBlock [BitsyExpr] -- [if | loop | break | print | read | assign]
                 | BIf String BitsyExpr BitsyExpr BitsyExpr -- keyword (IFZ | IFP | IFN) expr block else-block ended by END
                 | BElse BitsyExpr
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
                 | BMod String
                 | BInt Int
                 | BExpression BitsyExpr BitsyExpr BitsyExpr -- var1 operator var2
                 | BComment String
                 | BVoid
                 deriving Show

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
    expr <- (try parseExpr) <|> (try parseLiteral) <|> (try parseVar)
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
    tp <- (try parseExpr) <|> (try parseLiteral) <|> (try parseVar)
    newline
    return $ BPrint tp

  parseBreak :: Parser BitsyExpr
  parseBreak = spaces >> string "BREAK" >> newline >> return BBreak

  parseLoop :: Parser BitsyExpr
  parseLoop = do
    spaces
    string "LOOP"
    newline
    b <- parseBlock
    newlines
    spaces
    string "END"
    return $ BLoop b

  parseExpr :: Parser BitsyExpr
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

  parseIf :: Parser BitsyExpr
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

  parseElse :: Parser BitsyExpr
  parseElse = do
    string "ELSE"
    newline
    b1 <- parseBlock
    return $ BElse b1

  parseComment :: Parser BitsyExpr
  parseComment = do
    conts <- between (char '{') (char '}') (many (noneOf ['}']))
    return $ BComment conts

  parseBlock :: Parser BitsyExpr
  parseBlock = do
    exprs <- many1 $ (try parseIf) <|> (try parseLoop) <|> (try parseRead) <|> (try parsePrint) <|> (try parseAssign) <|> (try parseBreak) <|> (try parseExpr)
    return $ BBlock exprs

  parseProgram :: Parser BitsyExpr
  parseProgram = do
    comm <- parseComment
    newline
    string "BEGIN"
    newline
    mainBlock <- parseBlock
    newlines
    string "END"
    return $ BProgram comm mainBlock

  main :: IO ()
  main = do
    input <- readFile "test.bitsy"
    case parse parseProgram "error!" input of
      Right s -> putStrLn (show s)
      Left e -> putStrLn (show e)
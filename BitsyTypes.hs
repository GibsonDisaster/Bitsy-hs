module BitsyTypes where
  import Text.ParserCombinators.Parsec
  import Text.Parsec (Parsec, ParsecT, modifyState)
  import Data.Functor.Identity

  data BitsyExpr = BProgram BitsyExpr BitsyExpr BitsyExpr -- COMMENTS BEGIN block END VARLIST
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
                 | BError ParseError
                 | BDecVar BitsyExpr
                 | BVarList [BitsyExpr]
                 deriving (Show, Eq)

  data ParserState = ParserState {
    declaredVars :: [BitsyExpr],
    usedVars :: [BitsyExpr],
    isError :: Bool,
    errors :: [String]
  } deriving Show

  addUsedVar :: BitsyExpr -> ParserState -> ParserState
  addUsedVar s ps = ps { usedVars = (usedVars ps) ++ [s] }

  addDecVar :: BitsyExpr -> ParserState -> ParserState
  addDecVar s ps = ps { declaredVars = (declaredVars ps) ++ [s] }

  addErrorMsg :: String -> ParserState -> ParserState
  addErrorMsg s ps = ps { errors = (errors ps) ++ [s] }

  makeCrash :: ParserState -> ParserState
  makeCrash ps = ps { isError = True }
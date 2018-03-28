module BitsyGen where
  import BitsyTypes

  -- Recursively generate C code from a BitsyExpr

  compile :: BitsyExpr -> String
  compile (BProgram comments block decs) = compile comments ++ "#include <stdio.h>\nint main() {\n" ++ compile decs ++ compile block ++ "\n}"
  compile (BBlock exprs) = concat $ map compile exprs
  compile (BIf k expr ifblock elseblock) = "if (" ++ (compile expr) ++ (case k of { "IFP" -> "> 0"; "IFN" -> "< 0"; "IFZ" -> "== 0" })  ++ ") " ++ "{\n" ++ compile ifblock ++ "}" ++ "else {\n" ++ compile elseblock ++ "}"
  compile (BElse elseblock) = compile elseblock
  compile (BLoop block) = "while (1) {\n" ++ compile block ++ "}\n"
  compile BBreak = "break;\n"
  compile (BPrint toPrint) = "printf(\"%i\\n\", " ++ compile toPrint ++ ");\n"
  compile (BRead toRead) = compile toRead ++ ";\n" ++ "scanf (\"%d\", &" ++ (compile toRead) ++ ");\n"
  compile (BVarName s) = s
  compile (BAssign var expr) = compile var ++ " = " ++ compile expr ++ ";\n"
  compile (BAdd o) = o
  compile (BSub o) = o
  compile (BDiv o) = o
  compile (BMul o) = o
  compile (BMod o) = o
  compile (BInt i) = show i
  compile (BExpression v1 op v2) = compile v1 ++ " " ++ compile op ++ " " ++ compile v2
  compile (BComment s) = "/*\n " ++ (filter (\c -> c /= '\n') s) ++ "\n */ \n\n"
  compile BVoid = "\n"
  compile (BVarList vl) = concat $ map compile vl
  compile (BDecVar (BVarName s)) = "int " ++ s ++ ";\n"
  compile _ = "COMPILE-ERROR"
module CodeGen where

data CodeType = 
      Int
    | String
    | Double
    | Void
    deriving(Show)

data Literal = 
      IntLit Int
    | StringLit String
    | CharLit Char
    | Variable String
    deriving(Show)

data Argument = Argument String String
    deriving(Show)

data Operator = Equals | NotEquals | Plus | Minus | GreaterOrEquals
    deriving(Show)

data CodeToken = 
      Call String [Literal]
    | VarDecl String String [Literal]
    | Function CodeType String [Argument] [CodeToken]
    | Op Operator
    | WhileLoop [CodeToken] [CodeToken]
    | If [CodeToken] [CodeToken]
    | Else [CodeToken]
    | Value Literal
    | Semicolon
    | NL
    | Include String
    | Return [CodeToken]
    | Assigment String [CodeToken]
    deriving(Show)

tokenToCode :: CodeToken -> String
tokenToCode (Call name args) = name ++ "(" ++ litsToCode args ++ ")"
tokenToCode (VarDecl t n []) = t ++ " " ++ n
tokenToCode (VarDecl t n args) = t ++ " " ++ n ++ "(" ++ litsToCode args ++ ")"
tokenToCode (Function t n args code) = typeToCode t ++ " " ++ n ++ "(" ++ argsDecToCode args ++ "){\n" ++ tokensToCode code ++ "}\n"
tokenToCode (WhileLoop cond code) = "while(" ++ tokensToCode cond ++ "){\n"++ tokensToCode code ++"}\n"
tokenToCode (If cond code) = "if(" ++ tokensToCode cond ++ "){\n"++ tokensToCode code ++"}\n"
tokenToCode (Else code) = "else " ++ tokensToCode code
tokenToCode (Value lit) = litToCode lit
tokenToCode (Op op) = opToCode op
tokenToCode Semicolon = ";"
tokenToCode NL = "\n"
tokenToCode (Include v) = "#include " ++ v ++ "\n"
tokenToCode (Return code) = "return " ++ tokensToCode code ++ ";"
tokenToCode (Assigment var code) = var ++ "=" ++ tokensToCode code

tokensToCode :: [CodeToken] -> String
tokensToCode [] = ""
tokensToCode code = concatMap tokenToCode code

typeToCode :: CodeType -> String
typeToCode Int = "int"
typeToCode String = "String"
typeToCode Double = "double"
typeToCode Void = "void"

argsDecToCode :: [Argument] -> String
argsDecToCode [] = ""
argsDecToCode args = tail $ concatMap (("," ++) . argDecToCode) args 

argDecToCode :: Argument -> String
argDecToCode (Argument t n) = t ++ " " ++ n 

litsToCode :: [Literal] -> String
litsToCode [] = ""
litsToCode args = tail $ concatMap (("," ++) . litToCode) args

litToCode :: Literal -> String
litToCode (IntLit val) = show val
litToCode (StringLit val) = show val
litToCode (Variable val) = val
litToCode (CharLit val) = show val

opToCode :: Operator -> String
opToCode Equals = "=="
opToCode NotEquals = "!="
opToCode Plus = "+"
opToCode Minus = "-"
opToCode GreaterOrEquals = ">="

module Parsing where

import Exp
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
    ( haskellStyle, LanguageDef )
import Text.ParserCombinators.Parsec.Token
import Control.Applicative (some)
import Data.Char (isAlpha, isAlphaNum)
import Text.Parsec.String( parseFromFile)

miniHaskellDef :: LanguageDef st
miniHaskellDef = haskellStyle 
    {reservedNames = (reservedNames haskellStyle) ++ ["let", "letrec", "in"],
    reservedOpNames = (reservedOpNames haskellStyle) ++ ["=","->", "\\",":="] }


miniHs :: TokenParser st
miniHs = makeTokenParser  miniHaskellDef

m_parens = parens miniHs
m_identifier = identifier miniHs
m_reservedOp = reservedOp miniHs
m_reserved = reserved miniHs
m_semiSep1 = semiSep1 miniHs
m_semi = semi miniHs
m_whiteSpace = whiteSpace miniHs
m_commaSep = commaSep miniHs
m_brackets = brackets miniHs
m_natrual = natural miniHs
m_operator = operator miniHs

testParse :: Parser a -> String -> a
testParse p s
  = case parse p "<input>" s of
      Left err -> error (show err)
      Right a -> a

a1 = testParse digit "123"

var :: Parser Var
var =   do
    v <- m_identifier <|> m_operator
    return (Var v)
--var = do
--    m_whiteSpace
--    Var <$> (head <$> identStart)

-- >>> testParse var "b is a var"
-- Var {getVar = "b"}
a2 = testParse var "b + is a var"

varExp :: Parser ComplexExp
varExp = do CX <$> var
--varExp = do{x <- var;return $ CX $ x}

-- >>> testParse varExp "b is a var"
-- CX (Var {getVar = "b"})
a3 = testParse varExp "b is a var"

lambdaExp :: Parser ComplexExp
lambdaExp = do
       m_reservedOp "\\"
       x <- var
       m_reservedOp "->"
       CLam x <$> expr
-- >>> testParse lambdaExp "\\x -> x"
-- CLam (Var {getVar = "x"}) (CX (Var {getVar = "x"}))
a4 = testParse lambdaExp "\\x -> x"

letExp :: Parser ComplexExp
letExp = do
  m_reserved "let"
  x <- var
  m_reservedOp ":="
  y <- expr
  m_reserved "in"
  Let x y <$> expr
-- >>> testParse letExp "let x := y in z"
-- Let (Var {getVar = "x"}) (CX (Var {getVar = "y"})) (CX (Var {getVar = "z"}))
a5 = testParse letExp "let x := y in z"


letrecExp :: Parser ComplexExp
letrecExp = do
        m_reserved "letrec"
        x <- var
        m_reservedOp ":="
        y <- expr
        m_reserved "in"
        LetRec x y <$> expr

-- >>> testParse letrecExp "letrec x := y in z"
-- LetRec (Var {getVar = "x"}) (CX (Var {getVar = "y"})) (CX (Var {getVar = "z"}))
a6 =  testParse letrecExp "letrec x := y in z"

listExp :: Parser ComplexExp
listExp = List <$> m_brackets ( m_commaSep expr)


-- >>> testParse listExp "[a,b,c]"
-- List [CX (Var {getVar = "a"}),CX (Var {getVar = "b"}),CX (Var {getVar = "c"})]
a7 = testParse listExp "[a,b,c]"

natExp :: Parser ComplexExp
natExp = Nat <$> fromInteger <$> m_natrual


-- >>> testParse natExp "223 a"
-- Nat 223
a8 = testParse natExp "223 a"

parenExp :: Parser ComplexExp
parenExp =  m_parens expr
-- >>> testParse parenExp "(a)"
-- CX (Var {getVar = "a"})
a9 =  testParse parenExp "( a )"

basicExp :: Parser ComplexExp
basicExp = varExp <|> letExp <|> letrecExp <|> natExp <|> lambdaExp <|> listExp <|> parenExp
-- >>> testParse basicExp "[a,b,c]"
-- List [CX (Var {getVar = "a"}),CX (Var {getVar = "b"}),CX (Var {getVar = "c"})]
a10 = testParse basicExp "[a,b,c]"

expr :: Parser ComplexExp
expr = do { x <- some basicExp;
 return $ foldl (CApp) (head x) (tail x)}

-- >>> testParse expr "\\x -> [x,y,z]"
-- CLam (Var {getVar = "x"}) (List [CX (Var {getVar = "x"}),CX (Var {getVar = "y"}),CX (Var {getVar = "z"})])
a11 = testParse expr "\\x -> [x,y,z]"
a12 = testParse expr "x y z t"

exprParser :: Parser ComplexExp
exprParser = m_whiteSpace *> expr <* eof --whiteSpace miniHs *> expr <* eof
-- >>> testParse exprParser "let x := 28 in \\y -> + x y"
-- Let (Var {getVar = "x"}) (Nat 28) (CLam (Var {getVar = "y"}) (CApp (CApp (CX (Var {getVar = "+"})) (CX (Var {getVar = "x"}))) (CX (Var {getVar = "y"}))))
a13 = testParse exprParser "let x := 28 in \\y -> + x y"


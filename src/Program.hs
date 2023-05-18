module Program where
import Exp
import Parsing
import Printing
import Sugar ( desugarExp, desugarVar )
import Eval ( substitute )
import Text.Parsec.String (Parser)
import Text.Parsec(try)

import Control.Applicative ( Alternative(..) )
import System.IO ( stderr, hPutStrLn )
import qualified Data.Map.Strict as Map

data Definition = Definition
  { defHead :: Var
  , defArgs :: [Var]
  , defBody :: ComplexExp
  }
  deriving(Show)

definition :: Parser Definition
definition = do
  h <- m_identifier
  arg <- (many (m_identifier <|> m_operator ))
  m_reservedOp ":="
  ex <- expr
  return (Definition (Var h) (map Var arg) ex)

-- >>> parseFirst definition "id := \\x -> x"
-- testParse (definition) "id x := x"
-- Just (Definition {defHead = Var {getVar = "id"}, defArgs = [], defBody = CLam (Var {getVar = "x"}) (CX (Var {getVar = "x"}))})

-- >>> parseFirst definition "id x := x"
-- Just (Definition {defHead = Var {getVar = "id"}, defArgs = [Var {getVar = "x"}], defBody = CX (Var {getVar = "x"})})

-- >>> parseFirst definition "const x y := x"
-- Just (Definition {defHead = Var {getVar = "const"}, defArgs = [Var {getVar = "x"},Var {getVar = "y"}], defBody = CX (Var {getVar = "x"})})

semiSep11 :: Parser a -> Parser [a]
semiSep11 p = do
    a <- p
    ass <- many $ try (m_semi *> p)
    return (a : ass)

program :: Parser [Definition]
program = do {m_whiteSpace; x <- semiSep11 definition; m_semi; return x}

-- >>> parseFirst program "    id x := x ; const x y := x"
-- Nothing

-- >>> parseFirst program "    id x := x ; const x y := x ;"
-- Just [Definition {defHead = Var {getVar = "id"}, defArgs = [Var {getVar = "x"}], defBody = CX (Var {getVar = "x"})},Definition {defHead = Var {getVar = "const"}, defArgs = [Var {getVar = "x"},Var {getVar = "y"}], defBody = CX (Var {getVar = "x"})}]

definitionExp :: Definition -> ComplexExp
definitionExp (Definition h [] ex) = ex
definitionExp (Definition h (x:s) ex) = CLam (x) (definitionExp (Definition h s ex))

-- >>> definitionExp (Definition {defHead = Var {getVar = "const"}, defArgs = [Var {getVar = "x"},Var {getVar = "y"}], defBody = CX (Var {getVar = "x"})})
-- CLam (Var {getVar = "x"}) (CLam (Var {getVar = "y"}) (CX (Var {getVar = "x"})))

type Environment = Map.Map IndexedVar Exp

programEnv :: [Definition] -> Environment
programEnv [] = Map.empty
programEnv (x:xs) = Map.insert (desugarVar $ defHead x) (desugarExp $ definitionExp x) (programEnv xs)

normalizeEnv :: Environment -> Exp -> Exp
normalizeEnv env (X var)  = case Map.lookup var env of
        Nothing -> X var
        Just v -> v
normalizeEnv env (Lam var body)  = Lam var (normalizeEnv env body)
normalizeEnv env (App left right) = case normalizeEnv env left of
    Lam var body  -> normalizeEnv env (substitute var (normalizeEnv env right) body)
    otherwise -> App (normalizeEnv env left) (normalizeEnv env right)
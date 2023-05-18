
module REPLCommand where

import Data.Char
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef, LanguageDef)
import qualified Text.Parsec.Token as Token
import Text.Parsec (anyChar)

data REPLCommand
  = Quit
  | Load String
  | Eval String
 deriving Show

parseQuit :: Parser REPLCommand
parseQuit = do{(try  (string "quit") <|> (string "q")); many space; eof; return Quit;}

str c = not $ isSpace c

parseLoad :: Parser REPLCommand
parseLoad = do{(try (string "load") <|> (string "l")); many1 space; s <- (many1 $ satisfy str) ; many space; eof; return $ Load s;}

parseCom :: Parser REPLCommand
parseCom = do {char ':'; (parseQuit <|> parseLoad)}

parseEv :: Parser REPLCommand
parseEv = do{ s <- many anyChar ; return $ Eval s;}

replCommand :: Parser REPLCommand
replCommand = parseCom <|> parseEv


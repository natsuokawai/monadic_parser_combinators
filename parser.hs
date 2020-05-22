import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

-- 入力文字列から1文字消費
item :: Parser Char
item = P (\inp -> case inp of
                    []     -> []
                    (x:xs) -> [(x,xs)])

-- Functor
instance Functor Parser where
  -- fmap :: (a -> b) -> Parsera -> Parser b
  fmap g p = P (\inp -> case parse p inp of
                          []        -> []
                          [(v,out)] -> [(g v, out)])

-- Applicative
instance Applicative Parser where
  -- pure :: a -> Parser a
  -- 引数の値 v をパーサーに変換
  pure v = P (\inp -> [(v, inp)])

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case parse pg inp of
                           []        -> []
                           [(g,out)] -> parse (fmap g px) out)

-- Monad
instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of
                         []        -> []
                         [(v,out)] -> parse (f v) out)

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> [])

  --- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
                         []        -> parse q inp
                         [(v,out)] -> [(v,out)])

-- parse one character satisfying predicate
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat  (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)


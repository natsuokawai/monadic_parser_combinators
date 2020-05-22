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
  -- パースが成功したらパース結果に関数 g を適用する
  fmap g p = P (\inp -> case parse p inp of
                          []        -> []
                          [(v,out)] -> [(g v, out)])

-- Applicative
insatnce Applicative Parser where
  -- pure :: a -> Parser a
  -- 引数の値 v をパーサーに変換
  pure v = P (\inp -> [(v, inp)]

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case parse pg inp of
                           []        -> []
                           [(g,out)] -> parse (fmap g px) out)

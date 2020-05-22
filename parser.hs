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
-- パースが成功したらパース結果に関数 g を適用する
instance Functor Parser where
  -- fmap :: (a -> b) -> Parsera -> Parser b
  fmap g p = P (\inp -> case parser p inp of
                          []        -> []
                          [(v,out)] -> [(g v, out)]



module Main where

import qualified Text.Trifecta.Parser as Parser
import Text.Trifecta.Parser (Parser)
import qualified Data.Functor as Functor
import Data.Functor (Functor)
import qualified Control.Applicative as Applicative
import Control.Applicative (Applicative)
import qualified Text.Trifecta.Result as Result
import Text.Trifecta.Result (Result)
import qualified Text.PrettyPrint.ANSI.Leijen as Leijen

import qualified Text.Parser.Combinators as Combinators
import qualified Text.Parser.Char as Char
import qualified Data.Char

main :: IO ()
main = do
    let parser = Combinators.many $ Combinators.try number
    result <- Parser.parseFromFileEx parser "digits.txt"
    case result of
        Result.Failure doc -> Leijen.putDoc doc
        Result.Success a  -> print (sum a)

number :: Parser Int
number = do
    Combinators.skipMany Char.space
    digits <- Combinators.some digit
    return $ foldl (\x y -> x * 10 + y) (0::Int) digits

digit :: Parser Int
digit = Char.digit >>= (return . Data.Char.digitToInt)


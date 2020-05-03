--https://www.codingame.com/ide/puzzle/brackets-enhanced-edition
import Control.Monad
import Data.Either
import Data.Function
import Text.Parsec
import Text.Parsec.Char

type Parser = Parsec String ()

bracketChars = "()[]{}<>"

--check if a string contains valid pairs of (nested) brackets
isValid :: String -> Bool
isValid =
    isRight .  parse (many bracketPair <* eof) "" -- parse bracket pairs from string
    . filter (`elem` bracketChars) -- filter out non backets chars

-- a parser describing the structure of a valid bracket pair
bracketPair :: Parser ()
bracketPair = do
    firstBracket <- bracketChar
    many (try bracketPair) -- parse many bracket pairs in betwenn
    sencondBracket <- bracketChar
    guard (firstBracket `match` sencondBracket)

-- parsing one bracket char like { or (
bracketChar :: Parser Char
bracketChar = oneOf bracketChars

match :: Char -> Char -> Bool
match a b = kindOfBracket a == kindOfBracket b

kindOfBracket :: Char -> Int
kindOfBracket = flip div 10 . fromEnum

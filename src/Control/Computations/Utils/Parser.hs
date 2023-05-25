{-# OPTIONS_GHC -F -pgmF htfpp #-}

{- |
This module defines parser combinators for writing parsing for languages with
a syntax close to Haskell's. The parsers defined in this module share the following
convention with Megaparsec lexeme parsers:  every lexeme parser assumes no spaces
before the lexeme and consumes all spaces after the lexeme.
-}
module Control.Computations.Utils.Parser (
  stringLiteralP,
  intP,
  integerP,
  boolP,
  floatP,
  optionP,
  horizontalSpaceCharP,
  spaceP,
  lexemeP,
  symbolP,
  caseInsensitiveSymbolP,
  unknownSymbolP,
  listP,
  constrP,
  constrP',
  parensP,
  parensP',
  record1P,
  record2P,
  record3P,
  record4P,
  parseM,
  parseM',
  parserToReadsPrec,
  testParser,
  listS,
  optionalP,
  parserFromMapping,
  htf_thisModulesTests,
  -- Reexports
  Parser,
  oneOf,
  (<|>),
  (<?>),
  try,
  join,
)
where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.Utils.Fail
import Control.Computations.Utils.Types hiding (option)

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Arrow
import Control.Monad
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import Data.Void
import Test.Framework
import Test.QuickCheck.Instances.Text ()
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

type HaskellParser a =
  Int
  -- ^ the operator precedence of the enclosing context (a number from 0 to 11).
  -- Function application has precedence 10.
  -> Parser a

functionApplicationPrecedence :: Int
functionApplicationPrecedence = 10

parseM :: Parser a -> FilePath -> T.Text -> Fail a
parseM parser filename value =
  case parse (parser <* eof) filename value of
    Left err -> fail (errorBundlePretty err)
    Right x -> return x

parseM' :: Parser a -> String -> T.Text -> Fail (a, T.Text)
parseM' p fn str =
  let initialState =
        -- Unfortunately not exported by megaparsec
        State
          { stateInput = str
          , stateOffset = 0
          , stateParseErrors = []
          , statePosState =
              PosState
                { pstateInput = str
                , pstateOffset = 0
                , pstateSourcePos = initialPos fn
                , pstateTabWidth = defaultTabWidth
                , pstateLinePrefix = ""
                }
          }
      (s, mRes) = Text.Megaparsec.runParser' p initialState
   in case mRes of
        Left e -> fail $ errorBundlePretty e
        Right res -> return (res, stateInput s)

parserToReadsPrec :: Parser a -> String -> [(a, String)]
parserToReadsPrec p s =
  case parseM' p "" (T.pack s) of
    Fail e -> fail e
    Ok r -> [second T.unpack r]

testParser :: (Show a, Eq a) => T.Text -> Parser a -> a -> IO ()
testParser input p expected =
  case parseM p "<test>" input of
    Fail err -> assertFailure ("Parsing input " ++ show input ++ " failed: " ++ err)
    Ok x -> assertEqual expected x

testParserFailure :: (Show a) => T.Text -> Parser a -> IO ()
testParserFailure input p =
  case parseM p "<test>" input of
    Fail _err -> return ()
    Ok x -> assertFailure ("Parsing input " ++ show input ++ " succeeded as " ++ show x)

spaceP :: Parser ()
spaceP =
  L.space
    (void spaceChar)
    (fail "no support for line comments")
    (fail "no support for block comments")

horizontalSpaceCharP :: Parser ()
horizontalSpaceCharP =
  void (oneOf [' ', '\t'] <?> "horizontal space")

test_spaceP :: IO ()
test_spaceP =
  do testParser " \t \n" spaceP ()

lexemeP :: Parser a -> Parser a
lexemeP = L.lexeme spaceP

symbolP :: T.Text -> Parser ()
symbolP = void . L.symbol spaceP

caseInsensitiveSymbolP :: T.Text -> Parser ()
caseInsensitiveSymbolP = void . L.symbol' spaceP

unknownSymbolP :: Parser T.Text
unknownSymbolP =
  do
    first <- letterChar
    rest <- many (satisfy (\c -> isAlphaNum c || c == '_'))
    return $ T.pack (first : rest)

commaP :: Parser ()
commaP = symbolP ","

stringLiteralP :: Parser T.Text
stringLiteralP =
  T.pack <$> (lexemeP $ char '"' *> manyTill L.charLiteral (char '"'))

test_stringLiteralP :: IO ()
test_stringLiteralP =
  do
    subAssert $ testParser "\"*\"" stringLiteralP "*"
    subAssert $ testParser "\"\"" stringLiteralP ""
    subAssert $ testParser "\"* \\\" w\"" stringLiteralP "* \" w"
    subAssert $ testParser "\"hello world!\"  " stringLiteralP "hello world!"

prop_stringLiteralP :: T.Text -> Property
prop_stringLiteralP xs =
  case parse (stringLiteralP <* eof) "" (showText xs) of
    Left err ->
      counterexample (errorBundlePretty err) False
    Right ys -> xs === ys

integerP :: Parser Integer
integerP = L.signed space (lexemeP L.decimal)

intP :: Parser Int
intP =
  do
    n <- integerP
    return (fromInteger n)

test_intP :: IO ()
test_intP =
  do
    subAssert $ testParser "100 " intP 100
    subAssert $ testParser "-42 " intP (-42)

floatP :: Parser Double
floatP =
  try (L.signed space (lexemeP L.float))
    <|> do
      i <- intP
      return (fromInteger (toInteger i))

test_floatP :: IO ()
test_floatP =
  do
    subAssert $ testParser "100.00 " floatP (100.0 :: Double)
    subAssert $ testParser "100 " floatP (100 :: Double)
    subAssert $ testParser "-3.14 " floatP (-3.14 :: Double)
    subAssert $ testParser "-3 " floatP (-3 :: Double)

boolP :: Parser Bool
boolP =
  (symbolP "True" >> return True) <|> (symbolP "False" >> return False)

test_boolP :: IO ()
test_boolP =
  do
    subAssert $ testParser "True " boolP True
    subAssert $ testParser "False" boolP False

type Serializer a = a -> T.Text

listS :: Serializer a -> Serializer [a]
listS elemS xss =
  TL.toStrict . B.toLazyText $
    case xss of
      [] -> B.fromText "[]"
      x : xs -> B.fromText "[" <> B.fromText (elemS x) <> restS xs
 where
  restS yss =
    case yss of
      [] -> B.fromText "]"
      y : ys -> B.fromText "," <> B.fromText (elemS y) <> restS ys

listP :: Parser a -> Parser [a]
listP elemP =
  between (symbolP "[") (symbolP "]") $ sepBy (lexemeP elemP) (symbolP ",")

prop_listP :: [T.Text] -> Property
prop_listP xs =
  case parse (listP stringLiteralP <* eof) "" (showText xs) of
    Left err ->
      counterexample (errorBundlePretty err) False
    Right ys -> xs === ys

test_listP :: IO ()
test_listP =
  do
    subAssert $ testParser "[   ]   " (listP stringLiteralP) []
    subAssert $
      testParser
        "[ \"hello\"  , \"world\"  ]"
        (listP stringLiteralP)
        ["hello", "world"]

constrP :: T.Text -> Parser ()
constrP = symbolP

constrP' :: T.Text -> a -> Parser a
constrP' c x = symbolP c *> pure x

fieldP :: T.Text -> Parser a -> Parser a
fieldP name valueP =
  do
    symbolP name
    symbolP "="
    valueP

test_fieldP :: IO ()
test_fieldP =
  do subAssert $ testParser "foo =  42   " (fieldP "foo" intP) 42

parensP :: Parser a -> Parser a
parensP = between (symbolP "(") (symbolP ")")

parensP' :: Bool -> Parser a -> Parser a
parensP' withParens p =
  if withParens then parensP p else try (parensP p) <|> p

recordP :: T.Text -> Parser a -> Parser a
recordP consName p =
  do
    constrP consName
    between (symbolP "{") (symbolP "}") p

record1P :: T.Text -> (a -> b) -> (T.Text, Parser a) -> Parser b
record1P consName fun (f1, p1) =
  recordP consName $
    do
      x1 <- fieldP f1 p1
      return (fun x1)

record2P
  :: T.Text
  -> (a -> b -> c)
  -> (T.Text, Parser a)
  -> (T.Text, Parser b)
  -> Parser c
record2P consName fun (f1, p1) (f2, p2) =
  recordP consName $
    do
      x1 <- fieldP f1 p1
      commaP
      x2 <- fieldP f2 p2
      return (fun x1 x2)

test_record2P :: IO ()
test_record2P =
  do
    subAssert $
      testParser
        "Person {  name= \"Stefan\"  ,   age = 38 } "
        (record2P "Person" (\x y -> (x, y)) ("name", stringLiteralP) ("age", intP))
        ("Stefan", 38)

record3P
  :: T.Text
  -> (a -> b -> c -> d)
  -> (T.Text, Parser a)
  -> (T.Text, Parser b)
  -> (T.Text, Parser c)
  -> Parser d
record3P consName fun (f1, p1) (f2, p2) (f3, p3) =
  recordP consName $
    do
      x1 <- fieldP f1 p1
      commaP
      x2 <- fieldP f2 p2
      commaP
      x3 <- fieldP f3 p3
      return (fun x1 x2 x3)

record4P
  :: T.Text
  -> (a -> b -> c -> d -> e)
  -> (T.Text, Parser a)
  -> (T.Text, Parser b)
  -> (T.Text, Parser c)
  -> (T.Text, Parser d)
  -> Parser e
record4P consName fun (f1, p1) (f2, p2) (f3, p3) (f4, p4) =
  recordP consName $
    do
      x1 <- fieldP f1 p1
      commaP
      x2 <- fieldP f2 p2
      commaP
      x3 <- fieldP f3 p3
      commaP
      x4 <- fieldP f4 p4
      return (fun x1 x2 x3 x4)

optionalP :: a -> Parser a -> Parser a
optionalP def p =
  do
    x <- option def p
    spaceP
    return x

test_optional :: IO ()
test_optional =
  do
    subAssert $ testParser "" (optionalP def (return 2)) 2
    subAssert $ testParser "  " (optionalP def (return 2)) 2
    subAssert $ testParser "" (optionalP def (fail "blub")) 0
    subAssert $ testParser "  " (optionalP def (fail "blah")) 0
    let p =
          do
            i <- optionalP 0 intP
            b <- boolP
            return (i, b)
    subAssert $ testParser "True" p (0, True)
    subAssert $ testParser "42  True" p (42, True)
    let q = optionalP 0 intP
    subAssert $ testParser "42" q 42
    subAssert $ testParser "" q 0
 where
  def = 0 :: Int

optionP :: HaskellParser a -> HaskellParser (Option a)
optionP valueP enclosingPrec =
  noneP <|> (parensP' (enclosingPrec > functionApplicationPrecedence) (someP <|> noneP))
 where
  noneP = constrP "None" *> pure None
  someP =
    do
      constrP "Some"
      Some <$> (valueP $ functionApplicationPrecedence + 1)

test_option :: IO ()
test_option =
  do
    subAssert $ testParser "None" (optionP (const intP) 11) None
    subAssert $ testParser "Some 1" (optionP (const intP) 10) (Some 1)
    subAssert $ testParserFailure "Some b" (optionP (const intP) 10)
    subAssert $ testParserFailure "Some 1" (optionP (const intP) 11)
    subAssert $ testParser "(Some 1)" (optionP (const intP) 11) (Some 1)

parserFromMapping :: [(a, T.Text)] -> Parser a
parserFromMapping mapping =
  msum (map mkParser mapping)
 where
  mkParser (x, t) =
    do
      constrP t
      pure x

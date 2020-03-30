{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.MoreParser where

import Course.Core
import Course.Parser
import Course.List
import Course.Optional
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.Traversable

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Course.Parser(isErrorResult, character, lower, is)
-- >>> import Data.Char(isUpper, isLower)

-- | Parses the given input and returns the result.
-- The remaining input is ignored.
(<.>) ::
  Parser a
  -> Input
  -> Optional a
P p <.> i =
  case p i of
    Result _ a -> Full a
    _          -> Empty

-- | Write a parser that will parse zero or more spaces.
--
-- >>> parse spaces (listh " abc")
-- Result >abc< " "
--
-- >>> parse spaces (listh "abc")
-- Result >abc< ""
--
spaces ::
  Parser Chars
spaces = list space

-- | Write a function that applies the given parser, then parses 0 or more spaces,
-- then produces the result of the original parser.
--
-- /Tip:/ Use the monad instance.
--
-- >>> parse (tok (is 'a')) (listh "a bc")
-- Result >bc< 'a'
--
-- >>> parse (tok (is 'a')) (listh "abc")
-- Result >bc< 'a'
--
tok ::
  Parser a
  -> Parser a
tok a = (\x -> spaces >> valueParser x) =<< a

-- | Write a function that parses the given char followed by 0 or more spaces.
--
-- >>> parse (charTok 'a') (listh "abc")
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse (charTok 'a') (listh "dabc"))
-- True
--
-- /Tip:/ Use `tok` and `is`.
charTok ::
  Char
  -> Parser Char
charTok = tok . is

-- | Write a parser that parses a comma ',' followed by 0 or more spaces.
--
-- >>> parse commaTok (listh ",123")
-- Result >123< ','
--
-- >>> isErrorResult( parse commaTok (listh "1,23"))
-- True
--
-- /Tip:/ Use `charTok`.
commaTok ::
  Parser Char
commaTok = charTok ','

-- | Write a parser that parses either a double-quote or a single-quote.
--
-- /Tip:/ Use `is` and `|||`
--
-- >>> parse quote (listh "'abc")
-- Result >abc< '\''
--
-- >>> parse quote (listh "\"abc")
-- Result >abc< '"'
--
-- >>> isErrorResult (parse quote (listh "abc"))
-- True
--
quote ::
  Parser Char
quote = is '\'' ||| is '"'

-- | Write a function that parses the given string (fails otherwise).
--
-- /Tip:/ Use `is` and `traverse`.
--
-- >>> parse (string (listh "abc")) (listh "abcdef")
-- Result >def< "abc"
--
-- >>> isErrorResult (parse (string (listh "abc")) (listh "bcdef"))
-- True
--
string ::
  Chars
  -> Parser Chars
string = traverse is

-- | Write a function that parsers the given string, followed by 0 or more spaces.
--
-- /Tip:/ Use `tok` and `string`.
--
-- >>> parse (stringTok (listh "abc")) (listh "abc  ")
-- Result >< "abc"
--
-- >>> isErrorResult (parse (stringTok (listh "abc")) (listh "bc  "))
-- True
--
stringTok ::
  Chars
  -> Parser Chars
stringTok = tok . string

-- | Write a function that tries the given parser, otherwise succeeds by producing the given value.
--
-- /Tip:/ Use `|||`.
--
-- >>> parse (option 'x' character) (listh "abc")
-- Result >bc< 'a'
--
-- >>> parse (option 'x' character) (listh "")
-- Result >< 'x'
--
option ::
  a
  -> Parser a
  -> Parser a
option a x = x ||| valueParser a

-- | Write a parser that parses 1 or more digits.
--
-- /Tip:/ Use `list1` and `digit`.
--
-- >>> parse digits1 (listh "123")
-- Result >< "123"
--
-- >>> isErrorResult (parse digits1 (listh "abc123"))
-- True
--
digits1 ::
  Parser Chars
digits1 = list1 digit

-- | Write a function that parses one of the characters in the given string.
--
-- /Tip:/ Use `satisfy` and `elem`.
--
-- >>> parse (oneof (listh "abc")) (listh "bcdef")
-- Result >cdef< 'b'
--
-- >>> isErrorResult (parse (oneof (listh "abc")) (listh "def"))
-- True
--
oneof ::
  Chars
  -> Parser Char
oneof x = satisfy ( `elem` x)

-- | Write a function that parses any character, but fails if it is in the given string.
--
-- /Tip:/ Use `satisfy` and `notElem`.
--
-- >>> parse (noneof (listh "bcd")) (listh "abc")
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse (noneof (listh "abcd")) (listh "abc"))
-- True
--
noneof ::
  Chars
  -> Parser Char
noneof x = satisfy ( `notElem` x)

-- | Write a function that applies the first parser, runs the third parser keeping the result,
-- then runs the second parser and produces the obtained result.
--
-- /Tip:/ Use the monad instance.
--
-- >>> parse (between (is '[') (is ']') character) (listh "[a]")
-- Result >< 'a'
--
-- >>> isErrorResult (parse (between (is '[') (is ']') character) (listh "[abc]"))
-- True
--
-- >>> isErrorResult (parse (between (is '[') (is ']') character) (listh "[abc"))
-- True
--
-- >>> isErrorResult (parse (between (is '[') (is ']') character) (listh "abc]"))
-- True
--
between ::
  Parser o
  -> Parser c
  -> Parser a
  -> Parser a
between a b c = a >> c >>= (\x -> b >> valueParser x)

-- | Write a function that applies the given parser in between the two given characters.
--
-- /Tip:/ Use `between` and `charTok`.
--
-- >>> parse (betweenCharTok '[' ']' character) (listh "[a]")
-- Result >< 'a'
--
-- >>> isErrorResult (parse (betweenCharTok '[' ']' character) (listh "[abc]"))
-- True
--
-- >>> isErrorResult (parse (betweenCharTok '[' ']' character) (listh "[abc"))
-- True
--
-- >>> isErrorResult (parse (betweenCharTok '[' ']' character) (listh "abc]"))
-- True
--
betweenCharTok ::
  Char
  -> Char
  -> Parser a
  -> Parser a
betweenCharTok a b = between (charTok a) (charTok b)

-- | Write a function that parses 4 hex digits and return the character value.
--
-- /Tip:/ Use `readHex`, `isHexDigit`, `replicateA`, `satisfy` and the monad instance.
--
-- >>> parse hex (listh "0010")
-- Result >< '\DLE'
--
-- >>> parse hex (listh "0a1f")
-- Result >< '\2591'
--
-- >>> isErrorResult (parse hex (listh "001"))
-- True
--
-- >>> isErrorResult (parse hex (listh "0axf"))
-- True
--
hex ::
  Parser Char
hex = f <$> replicateA 4 (satisfy isHexDigit)
  where f x = case readHex x of
              Full a -> chr a
              Empty  -> chr 0

-- | Write a function that parses the character 'u' followed by 4 hex digits and return the character value.
--
-- /Tip:/ Use `is` and `hex`.
--
-- >>> parse hexu (listh "u0010")
-- Result >< '\DLE'
--
-- >>> parse hexu (listh "u0a1f")
-- Result >< '\2591'
--
-- >>> isErrorResult (parse hexu (listh "0010"))
-- True
--
-- >>> isErrorResult (parse hexu (listh "u001"))
-- True
--
-- >>> isErrorResult (parse hexu (listh "u0axf"))
-- True
--
hexu ::
  Parser Char
hexu = is 'u' >> hex

-- | Write a function that produces a non-empty list of values coming off the given parser (which must succeed at least once),
-- separated by the second given parser.
--
-- /Tip:/ Use `list` and the monad instance.
--
-- >>> parse (sepby1 character (is ',')) (listh "a")
-- Result >< "a"
--
-- >>> parse (sepby1 character (is ',')) (listh "a,b,c")
-- Result >< "abc"
--
-- >>> parse (sepby1 character (is ',')) (listh "a,b,c,,def")
-- Result >def< "abc,"
--
-- >>> isErrorResult (parse (sepby1 character (is ',')) (listh ""))
-- True
--
sepby1 ::
  Parser a
  -> Parser s
  -> Parser (List a)
sepby1 x y = x >>= (\a -> (a:.) <$> list (y >> x))

-- | Write a function that produces a list of values coming off the given parser,
-- separated by the second given parser.
--
-- /Tip:/ Use `sepby1` and `|||`.
--
-- >>> parse (sepby character (is ',')) (listh "")
-- Result >< ""
--
-- >>> parse (sepby character (is ',')) (listh "a")
-- Result >< "a"
--
-- >>> parse (sepby character (is ',')) (listh "a,b,c")
-- Result >< "abc"
--
-- >>> parse (sepby character (is ',')) (listh "a,b,c,,def")
-- Result >def< "abc,"
--
sepby ::
  Parser a
  -> Parser s
  -> Parser (List a)
sepby x y = sepby1 x y ||| valueParser Nil

-- | Write a parser that asserts that there is no remaining input.
--
-- >>> parse eof (listh "")
-- Result >< ()
--
-- >>> isErrorResult (parse eof (listh "abc"))
-- True
--
eof ::
  Parser ()
eof = () <$ P (\s -> if isEmpty s then Result s () else UnexpectedString s)

-- | Write a parser that produces a character that satisfies all of the given predicates.
--
-- /Tip:/ Use `sequence` and @Data.List#and@.
--
-- >>> parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) (listh "ABC")
-- Result >BC< 'A'
--
-- >>> parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) (listh "ABc")
-- Result >Bc< 'A'
--
-- >>> isErrorResult (parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) (listh "XBc"))
-- True
--
-- >>> isErrorResult (parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) (listh ""))
-- True
--
-- >>> isErrorResult (parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) (listh "abc"))
-- True
--
satisfyAll ::
  List (Char -> Bool)
  -> Parser Char
satisfyAll x = satisfy (and <$> sequence x) 

-- | Write a parser that produces a character that satisfies any of the given predicates.
--
-- /Tip:/ Use `sequence` and @Data.List#or@.
--
-- >>> parse (satisfyAny (isLower :. (/= 'X') :. Nil)) (listh "abc")
-- Result >bc< 'a'
--
-- >>> parse (satisfyAny (isLower :. (/= 'X') :. Nil)) (listh "ABc")
-- Result >Bc< 'A'
--
-- >>> isErrorResult (parse (satisfyAny (isLower :. (/= 'X') :. Nil)) (listh "XBc"))
-- True
--
-- >>> isErrorResult (parse (satisfyAny (isLower :. (/= 'X') :. Nil)) (listh ""))
-- True
--
satisfyAny ::
  List (Char -> Bool)
  -> Parser Char
satisfyAny = satisfy . (or <$>) . sequence

-- | Write a parser that parses between the two given characters, separated by a comma character ','.
--
-- /Tip:/ Use `betweenCharTok`, `sepby` and `charTok`.
--
-- >>> parse (betweenSepbyComma '[' ']' lower) (listh "[a]")
-- Result >< "a"
--
-- >>> parse (betweenSepbyComma '[' ']' lower) (listh "[]")
-- Result >< ""
--
-- >>> isErrorResult (parse (betweenSepbyComma '[' ']' lower) (listh "[A]"))
-- True
--
-- >>> isErrorResult (parse (betweenSepbyComma '[' ']' lower) (listh "[abc]"))
-- True
--
-- >>> isErrorResult (parse (betweenSepbyComma '[' ']' lower) (listh "[a"))
-- True
--
-- >>> isErrorResult (parse (betweenSepbyComma '[' ']' lower) (listh "a]"))
-- True
--
betweenSepbyComma ::
  Char
  -> Char
  -> Parser a
  -> Parser (List a)
betweenSepbyComma a b x = betweenCharTok a b (sepby x (charTok ','))

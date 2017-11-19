--
-- Chapter 12
--

import Data.List

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n then Just (n + 2) else Nothing

type Name = String
type Age = Integer
type ValidatePerson a = Either [PersonInvalid] a

data Person = Person Name Age deriving Show

------------------------------------------------------------------------------------

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Show)

instance Eq PersonInvalid where
  (==) NameEmpty NameEmpty = True
  (==) AgeTooLow AgeTooLow = True
  (==) _ _ = False

blah :: PersonInvalid -> String
blah pi
  | pi == NameEmpty = "Name empty"
  | pi == AgeTooLow = "Age too low"
  | otherwise       = "???"

ageOkay :: Age -> ValidatePerson Age
ageOkay age = case age >= 0 of
  True -> Right age
  False -> Left [AgeTooLow]

nameOkay :: Name ->  ValidatePerson Name
nameOkay name = case name /= "" of
  True -> Right name
  False -> Left [NameEmpty]

mkPerson ::  Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson'  (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right name) (Right age)     = Right (Person name age)
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson' (Left badName) _             = Left badName
mkPerson' _ (Left badAge)              = Left badAge

------------------------------------------------------------

data Unary a = Unary a deriving Show

--------------------
-- Chapter Exercises
--------------------

notThe :: String -> Maybe String
notThe str = case str of
  "the" -> Nothing
  otherwise -> Just str

fromMaybe' :: a -> Maybe a -> a
fromMaybe' a (Just b) = b
fromMaybe' a (Nothing) = a

-- replaceThe "the cow loves us"
-- "a cow loves us"
replaceThe :: String -> String
replaceThe str = res
  where
    w = map (fromMaybe' "a") $ map notThe (words str)
    res = concat $ intersperse " " w

-- Write a recursive function that takes a text/string, breaks it into
-- words, and counts the number of instances of ”the” followed by
-- a vowel-initial word.
--
-- countTheBeforeVowel "the cow"
-- 0
-- countTheBeforeVowel "the evil cow"
-- 1
-- "the air show with the owl"
-- 2
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = foldr f 0 (zip w (tail w))
  where
    w = words str
    f :: (String, String) -> Integer -> Integer
    f (s1, (s : s2)) acc
      | s1 == "the" && s `elem` "aeiou" = 1 + acc
      | otherwise                       = acc

-- Return the number of letters that are vowels in a word.
-- Hint: it’s helpful to break this into steps. Add any helper func-
-- tions necessary to achieve your objectives.
-- a) Test for vowelhood
-- b) Return the vowels of a string
-- c) Count the number of elements returned
-- countVowels "the cow"
-- 2
-- countVowels "Mikolajczak"
-- 4
countVowels :: String -> Integer
countVowels  = fromIntegral . length . filter isVowel

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"


--Validate the word
-------------------
-- Use the Maybe type to write a function that counts the number of
-- vowels in a string and the number of consonants. If the number
-- of vowels exceeds the number of consonants, the function returns
-- Nothing. In many human languages, vowels rarely exceed the number
-- of consonants so when they do, it may indicate the input isn’t a word
-- (that is, a valid input to your dataset):

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord = undefined


  

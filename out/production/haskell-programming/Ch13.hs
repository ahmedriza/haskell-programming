--
-- Chapter 13
--

-- Exercises

import Control.Monad
import System.Exit (exitSuccess, exitFailure)
import Data.Char (toLower)

alphabet = ['a'..'z']

palindrome :: IO ()
palindrome = forever $ do
  line' <- getLine
  let line = (removeSpaces . removePunctuation) $ map toLower line'
  case (line == reverse line) of
    True -> putStrLn "It's a palindrome!"
    False -> putStrLn "Nope" >>  exitFailure
  
palindromeTest = "Madam I'm Adam" -- This should be a palindrome

-- Remove spaces
removeSpaces :: String -> String
removeSpaces = filter (/= ' ')

-- Remove punctuation
removePunctuation :: String -> String
removePunctuation = filter $ (flip  elem) alphabet

----------------

type Name = String
type Age = Integer

data Person = Person Name Age deriving (Show)

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age >= 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ ", age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  name <- getLine
  age <- getLine
  case mkPerson name (read age::Integer) of
    Right person -> putStrLn("Yay!, Successfully got a person: " ++ show person)
    Left NameEmpty -> putStrLn("An error occurred: name empty")
    Left AgeTooLow -> putStrLn("An error occurred: age too low")
    Left (PersonInvalidUnknown e) -> putStrLn("An error occurred: " ++ e)
  
  return ()
                

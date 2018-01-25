module MonadIO where

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class

main :: IO ()
main = do
  password <- runMaybeT getPassword
  print password
  return()

getPassword :: MaybeT IO String
getPassword = do
  lift $ putStrLn "Enter password"
  password <- lift getLine
  guard (isValid password)
  return password

isValid :: String -> Bool
isValid = (>= 3) . length

eitherTest :: ExceptT String IO Int
eitherTest = undefined

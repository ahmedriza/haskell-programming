{-# LANGUAGE OverloadedStrings #-}
--
-- In GHCI, do
-- :set -XOverloadedStrings
--

module MonadTransformers where

import Data.Text
import qualified Data.Text.IO as T
import Data.Map as Map
import Control.Applicative

data LoginError = InvalidEmail
                | NoSuchUser
                | WrongPassword
  deriving Show

getDomain :: Text -> Either LoginError Text
getDomain email =
  case splitOn "@" email of
    [name, domain] -> Right domain
    _              -> Left InvalidEmail

printResult' :: Either LoginError Text -> IO ()
printResult' domain = case domain of
  Right text        -> T.putStrLn (append "Domain: " text)
  Left InvalidEmail -> T.putStrLn "ERROR: Invalid domain"

printResult :: Either LoginError Text -> IO ()
printResult = T.putStrLn . either
  (const "ERROR: Invalid domain")
  (append "Domain: ")

getToken :: IO (Either LoginError Text)
getToken = do
  T.putStrLn "Enter email address:"
  email <- T.getLine
  return (getDomain email)

users :: Map Text Text
users = Map.fromList [("example.com", "qwerty123"), ("localhost", "password")]

userLogin :: IO (Either LoginError Text)
userLogin = do
  token <- getToken
  case token of
    Right domain ->
      case Map.lookup domain users of
        Just userpw -> do
          T.putStrLn "Enter password:"
          password <- T.getLine

          if userpw == password
            then return token

            else return (Left WrongPassword)
        Nothing -> return (Left NoSuchUser)
    left -> return left

testDomain = printResult' $ getDomain "ahmed@gmail.com"

------------------------------------------------------------------------

data EitherIO e a = EitherIO {
  runEitherIO :: IO (Either e a)
  }

instance Functor (EitherIO e) where
  fmap f ex = wrapped
    where
      unwrapped = runEitherIO ex
      fmapped = fmap (fmap f) unwrapped
      wrapped = EitherIO fmapped

instance Applicative (EitherIO e) where
  pure = EitherIO . return . Right
  f <*> x = EitherIO $ liftA2 (<*>) f' x'
    where
      f' = runEitherIO f -- IO (Either e (a -> b))
      x' = runEitherIO x -- IO (Either e a)

instance Monad (EitherIO e) where
  return = pure
  x >>= f = EitherIO $ do
    x' <- runEitherIO x
    case x' of
      Left e -> return (Left e)
      Right x'' -> runEitherIO (f x'')

-- (>>=) :: Monad m => m            a -> (a         -> m           b) -> m  b
-- (>>=) ::            IO (Right Int) -> (Right Int -> IO (Right Int) -> IO (Right Int)

f :: IO (Either String (Integer -> Integer))
f = return $ Right (\i -> i * 10)

-- ff :: Either String Int -> IO (Either String Int)
-- ff (Left x) f = return $ Left x

x :: IO (Either String Integer)
x = return $ Right 10

monadEx :: IO (Either String Integer) -> IO (Either String Integer)
monadEx y = y >>= (\x' -> return $ fmap (+1) x')

-- <*> :: f (a -> b) -> f a -> f b
--
-- liftA2 :: (        a  ->   b ->   c) -> f  a            -> f          b           -> f  c
-- liftA2 :: (f (a -> b) -> f a -> f b) -> f  (f     (a    -> b))  -> f  (f       a) -> f  (f     b)
-- liftA2 :: (<*>)                      -> IO (Right (Int -> Int)) -> IO (Right Int) -> IO (Right Int)


liftEither :: Either e a -> EitherIO e a
liftEither x = EitherIO (return x)

liftIO :: IO a -> EitherIO e a
liftIO x = EitherIO (fmap Right x)

getToken' :: EitherIO LoginError Text
getToken' = do
  liftIO (T.putStrLn "Enter email address:")
  input <- liftIO T.getLine
  liftEither (getDomain "")

userLogin' :: EitherIO LoginError Text
userLogin' = do
  token <- getToken'
  userpw <- maybe (liftEither (Left NoSuchUser)) return (Map.lookup token users)
  password <- liftIO (T.putStrLn "Enter your password:"  >> T.getLine)

  if userpw == password
    then return token
    else liftEither (Left WrongPassword)

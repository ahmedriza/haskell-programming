{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List

import Control.Monad (join, (>=>), liftM2)

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Web.Scotty hiding (header, Options)

import Data.Monoid (mconcat)
import Data.Semigroup ((<>), Semigroup)

import qualified Data.Map as M

import Options.Applicative

import Network.Socket

import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class -- lift comes from here

import System.Environment (getArgs)
import Control.Error.Safe (tryHead)

--
-- Chapter 19
--

scottyTest :: IO ()
scottyTest = scotty 3000 $
  (get "/" $ do
      html "hello there"
  )
  >>
  (get "/hello" $ do
      name <- param "name"
      html $ mconcat ["<h1>Hello ", name, "!</h1>"]
  )
  >>
  (get "/:word" $ do
      beam <- param "word"
      html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
  )
  >>
  (notFound $ do
      text "Not Found"
  )

fm = M.fromList [('a', 1)]
gm = M.fromList [('b', 2)]

--------------------------------
-- optparse-applicative example

data Sample = Sample
  { hello      :: String
  , quiet      :: Bool
  , enthusiasm :: Int }
  deriving (Show)

sample'' :: Parser Sample
sample'' = Sample
           <$> strOption
           (long "hello" <> metavar "TARGET" <> help "Target for the greeting")
           <*> switch
           (long "quiet" <> short 'q' <> help "Whether to be quiet")
           <*> option auto
           (long "enthusiasm" <> help "How enthusiastically to greet"
             <> showDefault
             <> value 1
             <> metavar "INT"
           )

greet :: Sample -> IO ()
greet (Sample h False n) = Prelude.putStrLn $ "Hello, " ++ h ++
  Prelude.replicate n '!'
greet _ = return ()          

main :: IO ()
main = greet =<< execParser opts
  where
    opts =
      info (helper <*> sample'')
      (fullDesc <> progDesc "Print a greeting for TARGET" <>
       header "hello - a test for optparse-applicative")

-----

type App = String
type Version = String
type Url = String
type BuildId = String

data Command = Start Url Version
             | Status BuildId
             | Release BuildId App
             deriving Show

data Options = Options App Command deriving Show

runner :: IO ()
runner = run =<< execParser (withInfo parseOptions "testing  123")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseApp :: Parser App
parseApp = strOption $
  short 'a' <> long "app" <> metavar "COMPILE-APP" <>
  help "Heroku app on which to compile"

parseStart :: Parser Command
parseStart = Start
  <$> argument str (metavar "SOURCE-URL")
  <*> argument str (metavar "VERSION")

parseStatus :: Parser Command
parseStatus = Status <$> argument str (metavar "BUILD-ID")

parseRelease :: Parser Command
parseRelease = Release
  <$> argument str (metavar "BUILD-ID")
  <*> argument str (metavar "RELLEASE-APP")

parseCommand :: Parser Command
parseCommand = subparser $
  command "start" (withInfo parseStart "Start a build") <>
  command "status" (withInfo parseStatus "Check build status") <>
  command "release" (withInfo parseRelease "Release a successful build")

parseOptions :: Parser Options
parseOptions = Options <$> parseApp <*> parseCommand

run :: Options -> IO ()
run (Options app cmd) = do
  case cmd of
    Start url version -> print url
    Status build -> print build
    Release build rApp -> print rApp
  
               
--------------------------------

(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<||>) = liftA2 (||)

-- liftA2 (a -> b -> c) -> f           a -> f           b -> f       c
--
-- f = (-> a)
--
-- liftA2 (a -> b -> c) -> ((-> a) Bool) -> ((-> a) Bool) -> ((-> a) Bool
-- liftA2 (a -> b -> c) -> (a -> Bool)   -> (a -> Bool)   -> (a -> Bool)

network :: IO ()
network = do
  let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV],
                             addrSocketType = Stream }
  addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "5000")
  print addr
  sock@(MkSocket fd family stype proto status) <- socket (addrFamily addr)
    (addrSocketType addr) (addrProtocol addr)
  print fd
  print family
  print proto
  return ()

---------

eitherTest :: IO ()
eitherTest = do
  runEitherT $ do
    t <- lift getLine
    return ()
  return ()

--
getA :: IO (Maybe Int)
getA = return (Just 10)

getB :: Int -> IO (Maybe Int)
getB x = return (Just (x*10))

getC :: IO (Int)
getC = return 100

maybeTest :: IO (Maybe (Int, Int, Int, Int))
maybeTest = do
  c1 <- getC
  runMaybeT $ do
    a <- MaybeT getA
    b <- MaybeT $ getB 2
    c2 <- lift getC
    return (a, b, c1, c2)

data Error = Error deriving Show

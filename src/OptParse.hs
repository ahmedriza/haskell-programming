{-# LANGUAGE Rank2Types, ExistentialQuantification #-}

module OptParse where

import Data.Monoid
import Control.Applicative
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.Trans.Except (Except, throwE)
import Control.Monad (ap, liftM, MonadPlus, mzero, mplus)
import Control.Monad.Trans.Class (lift)

class HasName f where
  name :: OptName -> f a -> f a

class HasMetavar f where
  hasMetavarDummy :: f a -> ()

---------------

data Mod f a = Mod
               (f a -> f a)
               (DefaultProp a)
               (OptProperties -> OptProperties)

instance Monoid (Mod f a) where
  mempty = Mod id mempty id
  mappend = (<>)

newtype Completer = Completer {
  runCompleter :: String -> IO [String]
  }
  
data OptName = OptShort !Char
             | OptLong !String
  deriving (Eq, Ord, Show)

data OptionFields a = OptionFields {
  optNames :: [OptName]
  , optCompleter :: Completer
  , optNoArgError :: ParseError
  }

instance HasName OptionFields where
  name n fields = fields { optNames = n : optNames fields }

instance HasMetavar OptionFields where
  hasMetavarDummy _ = ()

data ArgumentFields a = ArgumentFields
  { argCompleter :: Completer }

instance HasMetavar ArgumentFields where
  hasMetavarDummy _ = ()

data FlagFields a = FlagFields {
  flagNames :: [OptName]
  , flagActive :: a
  }
  deriving (Show)

instance HasName FlagFields where
  name n fields = fields { flagNames = n : flagNames fields }

instance Monoid Completer where
  mempty = Completer $ \_ -> return []
  mappend (Completer c1) (Completer c2) =
    Completer $ \s -> (++) <$> c1 s <*> c2 s

data DefaultProp a = DefaultProp
  (Maybe a)
  (Maybe (a -> String))

instance Monoid (DefaultProp a) where
  mempty = DefaultProp Nothing Nothing
  mappend = (<>)

-- | Visibility of an option in the help text.
data OptVisibility
  = Internal          -- ^ does not appear in the help text at all
  | Hidden            -- ^ only visible in the full description
  | Visible           -- ^ visible both in the full and brief descriptions
  deriving (Eq, Ord, Show)

data OptProperties = OptProperties {
  propVisibility :: OptVisibility         -- ^ whether this flag is shown is the brief description
  , propMetaVar :: String                 -- ^ metavariable for this option
  , propShowDefault :: Maybe String       -- ^ what to show in the help text as the default
  , propHelp :: String
  } deriving Show

-- | A full description for a runnable 'Parser' for a program.
data ParserInfo a = ParserInfo { 
  infoFullDesc :: Bool  
  , infoProgDesc :: String
  }
  deriving Show

newtype InfoMod a = InfoMod {
  applyInfoMod :: ParserInfo a -> ParserInfo a
  }

-- | Specify a short program description.
progDesc :: String -> InfoMod a
progDesc s = InfoMod $ \i -> i { infoProgDesc = s }

base = ParserInfo {
  infoFullDesc   = False
  , infoProgDesc = mempty
  }

-- | Create a 'ParserInfo' given a 'Parser' and a modifier.
{--
info :: Parser a -> InfoMod a -> ParserInfo a
info parser m = applyInfoMod m base
  where
    base = ParserInfo
      { infoParser = parser
      , infoFullDesc = True
      , infoProgDesc = mempty
      , infoHeader = mempty
      , infoFooter = mempty
      , infoFailureCode = 1
      , infoIntersperse = True }
--}

-- | Builder for an option taking a 'String' argument.
-- strOption :: Mod OptionFields String -> Parser String
-- strOption = option str

data ParseError
  = ErrorMsg String
  | InfoMsg String
  | ShowHelpText
  | UnknownError

instance Monoid ParseError where
  mempty = UnknownError
  mappend = (<>)

-- | A newtype over 'ReaderT String Except', used by option readers.
newtype ReadM a = ReadM {
  unReadM :: ReaderT String (Except ParseError) a
  }

instance Functor ReadM where
  fmap f (ReadM r) = ReadM (fmap f r)

instance Applicative ReadM where
  pure = ReadM . pure
  ReadM x <*> ReadM y = ReadM $ x <*> y

instance Alternative ReadM where
  empty = mzero
  (<|>) = mplus

instance Monad ReadM where
  return = pure
  ReadM r >>= f = ReadM $ r >>= unReadM . f
  fail = readerError

instance MonadPlus ReadM where
  mzero = ReadM mzero
  mplus (ReadM x) (ReadM y) = ReadM $ mplus x y

-- | Abort option reader by exiting with a 'ParseError'.
readerAbort :: ParseError -> ReadM a
readerAbort = ReadM . lift . throwE

-- | Abort option reader by exiting with an error message.
readerError :: String -> ReadM a
readerError = readerAbort . ErrorMsg

data CReader a = CReader {
  crCompleter :: Completer
  , crReader :: ReadM a
  }

instance Functor CReader where
  fmap f (CReader c r) = CReader c (fmap f r)

-------

optionMod :: (OptProperties -> OptProperties) -> Mod f a
optionMod = Mod id mempty

fieldMod :: (f a -> f a) -> Mod f a
fieldMod f = Mod f mempty id

-- | Specify a long name for an option.
long :: HasName f => String -> Mod f a
long = fieldMod . name . OptLong

-- | Specify a short name for an option.
short :: HasName f => Char -> Mod f a
short = fieldMod . name . OptShort

-- | Specify the help text for an option.
help :: String -> Mod f a
help s = optionMod $ \p -> p { propHelp = s }

flagFields :: FlagFields Integer
flagFields = FlagFields { flagNames = [OptShort 'c'], flagActive = 20 }

data OptReader a
  = OptReader [OptName] (CReader a) ParseError          -- ^ option reader
  | FlagReader [OptName] !a                             -- ^ flag reader
  | ArgReader (CReader a)                               -- ^ argument reader
  | CmdReader (Maybe String)
              [String] (String -> Maybe (ParserInfo a)) -- ^ command reader

instance Functor OptReader where
  fmap f (OptReader ns cr e) = OptReader ns (fmap f cr) e
  fmap f (FlagReader ns x) = FlagReader ns (f x)
  fmap f (ArgReader cr) = ArgReader (fmap f cr)

-- | A single option of a parser.
data Option a = Option
  { optMain :: OptReader a               -- ^ reader for this option
  , optProps :: OptProperties            -- ^ properties of this option
  }

instance Show (Option a) where
    show opt = "Option {optProps = " ++ show (optProps opt) ++ "}"

instance Functor Option where
  fmap f (Option m p) = Option (fmap f m) p

-- | A @Parser a@ is an option parser returning a value of type 'a'.
data Parser a
  = NilP (Maybe a)
  | OptP (Option a)
  | forall x . MultP (Parser (x -> a)) (Parser x)
  | AltP (Parser a) (Parser a)
  | forall x . BindP (Parser x) (x -> Parser a)

instance Functor Parser where
  fmap f (NilP x) = NilP (fmap f x)
  fmap f (OptP opt) = OptP (fmap f opt)
  fmap f (MultP p1 p2) = MultP (fmap (f.) p1) p2
  fmap f (AltP p1 p2) = AltP (fmap f p1) (fmap f p2)
  fmap f (BindP p k) = BindP p (fmap f . k)

newtype ParserM r = ParserM
  { runParserM :: forall x . (r -> Parser x) -> Parser x }

instance Monad ParserM where
  return x = ParserM $ \k -> k x
  ParserM f >>= g = ParserM $ \k -> f (\x -> runParserM (g x) k)

instance Functor ParserM where
  fmap = liftM

instance Applicative ParserM where
  pure = return
  (<*>) = ap

fromM :: ParserM a -> Parser a
fromM (ParserM f) = f pure

oneM :: Parser a -> ParserM a
oneM p = ParserM (BindP p)

manyM :: Parser a -> ParserM [a]
manyM p = do
  mx <- oneM (optional p)
  case mx of
    Nothing -> return []
    Just x -> (x:) <$> manyM p

someM :: Parser a -> ParserM [a]
someM p = (:) <$> oneM p <*> manyM p

instance Alternative Parser where
  empty = NilP Nothing
  (<|>) = AltP
  many p = fromM $ manyM p
  some p = fromM $ (:) <$> oneM p <*> manyM p

instance Applicative Parser where
  pure = NilP . Just
  (<*>) = MultP

-- | Create a parser composed of a single option.
liftOpt :: Option a -> Parser a
liftOpt = OptP

mkParser :: DefaultProp a
         -> (OptProperties -> OptProperties)
         -> OptReader a
         -> Parser a
mkParser d@(DefaultProp def _) g rdr = liftOpt opt <|> maybe empty pure def
  where
    opt = mkOption d g rdr

mkOption :: DefaultProp a
         -> (OptProperties -> OptProperties)
         -> OptReader a
         -> Option a
mkOption d g rdr = Option rdr (mkProps d g)

mkProps :: DefaultProp a
        -> (OptProperties -> OptProperties)
        -> OptProperties
mkProps (DefaultProp def sdef) g = props
  where
    props = (g baseProps)
      { propShowDefault = sdef <*> def }

-- | Base default properties.
baseProps :: OptProperties
baseProps = OptProperties { propMetaVar = ""
  , propVisibility = Visible
  , propHelp = mempty
  , propShowDefault = Nothing
  }

-- | Specify a metavariable for the argument.
--
-- Metavariables have no effect on the actual parser, and only serve to specify
-- the symbolic name for an argument to be displayed in the help text.
metavar :: HasMetavar f => String -> Mod f a
metavar var = optionMod $ \p -> p { propMetaVar = var }

option :: ReadM a -> Mod OptionFields a -> Parser a
option r m = mkParser d g rdr
  where
    Mod f d g = metavar "ARG" `mappend` m
    fields = f (OptionFields [] mempty (ErrorMsg ""))
    crdr = CReader (optCompleter fields) r
    rdr = OptReader (optNames fields) crdr (optNoArgError fields)

-- | String 'Option' reader.
str :: ReadM String
str = readerAsk

-- | Return the value being read.
readerAsk :: ReadM String
readerAsk = ReadM ask

-- | Builder for an option taking a 'String' argument.
strOption :: Mod OptionFields String -> Parser String
strOption = option str

-- | Builder for an argument parser.
argument :: ReadM a -> Mod ArgumentFields a -> Parser a
argument p (Mod f d g) = mkParser d g (ArgReader rdr)
  where
    ArgumentFields compl = f (ArgumentFields mempty)
    rdr = CReader compl p

---

mods :: Mod OptionFields String
mods = short 'a' -- <> long "application"


newtype Reader e a = Reader { runReader :: e -> a }

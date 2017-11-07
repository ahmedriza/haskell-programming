--
-- Chapter 11
--

type AuthorName = String

data Author = Fiction AuthorName
            | NonFiction AuthorName
            deriving (Eq, Show)

ahmed = NonFiction "ahmed"

john = Fiction "john"

------------------------------------------------

data Expr = Number Int
          | Add Expr Expr
          | Minus Expr
          | Mult Expr Expr
          | Divide Expr Expr
          deriving Show

apply :: Expr -> Expr
apply (Number n) = (Number n)
apply (Add (Number n1) (Number n2)) = Number (n1 + n2)
apply (Minus (Number n)) = Number (-n)
apply (Mult (Number n1) (Number n2)) = Number (n1 * n2)
apply (Divide (Number n1) (Number n2)) = Number (div n1 n2)

-- Exercises

{--
data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show

data Garden = Garden Gardener FlowerType deriving Show
--}

type Gardener = String

-- What is the sum of products normal form of Garden?

data Garden = Gardenia Gardener
            | Daisy Gardener
            | Rose Gardener
            | Lilac Gardener
            deriving Show

data GuessWhat = Chickenbutt deriving (Eq, Show)

data Id a = MkId a deriving (Eq, Show)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

data RecordProduct a b = RecordProduct { pfirst :: a, psecond :: b } deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)

newtype NumPig = NumPig Int deriving (Eq, Show)

data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep = NumSheep Int deriving (Eq, Show)

data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)

type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool
type PoundsOfWool = Int

data CowInfo = CowInfo Name Age deriving (Eq, Show)

data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)

data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)

data Animal = Cow CowInfo
            | Pig PigInfo
            | Sheep SheepInfo
            deriving (Eq, Show)

-- alternatively
type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

-- animal = First (CowInfo "bill" 2) (First (PigInfo "pepper" 1)

bess' = (CowInfo "Bess" 4)
bess = First bess' :: Animal'

elmer' = Second (SheepInfo "Elmer" 5 5)
elmer =  Second elmer' :: Animal'

sheep = SheepInfo "Baaa" 5 5

-- ===================
-- Constructing Values
-- ===================

trivialValue :: GuessWhat
trivialValue = Chickenbutt

idInt :: Id Integer
idInt = MkId 10

type Awesome = Bool

person :: Product Name Awesome
person = Product "Simon" True

data Twitter = Twitter deriving (Eq, Show)

data AskFm = AskFm deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter

data SocialNetwork = Twitter'
                   | AskFm'
                   deriving (Eq, Show)

----

type Twitter'' = String
type AskFm'' = String

twitter :: Sum Twitter'' AskFm''
twitter = First "Twitter"

askfm :: Sum Twitter'' AskFm''
askfm = First "AskFm"

myRecord :: RecordProduct Integer Float
myRecord = RecordProduct { pfirst = 42, psecond = 0.001 }

---

data OperatingSystem = GnuPlusLinux
                     | OpenBSDPlusNevermindJustBSDStill
                     | Mac
                     | Windows
                     deriving (Eq, Show)

data ProgLang = Haskell
              | Agda
              | Idris
              | PureScript
              deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem, lang :: ProgLang } deriving (Eq, Show)

-- Write a function that generates all possible values of Programmer.  Use
-- the provided list of inhabitants of OperatingSystem and ProgLang

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux
                      , OpenBSDPlusNevermindJustBSDStill
                      , Mac
                      , Windows
                      ]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [ Programmer os lang | os <- allOperatingSystems, lang <- allLanguages ]







  



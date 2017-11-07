--
-- Chapter 11
--

-- =====================
-- Deconstructing Values
-- =====================

newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show

data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer
                deriving Show

data Farmer = Farmer Name Acres FarmerType deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

jim = Farmer (Name "jim") (Acres 20) WheatFarmer
bob = Farmer (Name "bob") (Acres 30) DairyFarmer

data FarmerRec = FarmerRec { name :: Name, acres :: Acres, farmerType :: FarmerType }

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer = case farmerType farmer of
                         DairyFarmer -> True
                         otherwise -> False

                         
-- ===================
-- Higher Kinded Types
-- ===================

data ElasticSearchResultFound a = ElasticSearchResultFound {
  _version :: String
  , _source :: a
  } deriving (Eq, Show)


--
-- Lists are polymorphic
-- 

data List a = Nil | Cons a (List a) deriving (Eq, Show)

-- ===========
-- Binary Tree
-- ===========

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

-- Inserting into trees

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

-- Write map for Binary Tree

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay = if mapTree (+1) testTree' == mapExpected
             then print "yup okay!"
                  else error "test failed!"
  

-- Convert binary trees to lists

-- Write functions to convert BinaryTree values to lists.  Make certain
-- your implementation passes the tests.




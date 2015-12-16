-- ch 3
-- p 41

data BookInfo = Book Int String [String]
      deriving (Show)

-- Here, BookInfo is the name of our new type, we call this a Type Constructor.
-- The Book that follows is the name of the value constructor (sometimes called
-- a data constructor). After Book, the Int, String, and [String] that follow
-- are the compontents of the type. A component serves the same purpose in
-- Haskell as a field in the structure or class would in another language.

-- The value constructor can be trated as just another function, in this case
-- :type Book yields: Book :: Int -> String -> [String] -> BookInfo

data MagazineInfo = Magazine Int String [String]
      deriving (Show)

-- Even though this magazine info type has the same structure as our BookInfo
-- type, Haskell treats the tpyes as distinct because their type and value
-- constructors have different names

myInfo = Book 1 "Algebra of Programming" ["Richard Bird", "Oege de Moor"]

-- data BookReview = BookReview BookInfo CustomerID String
type CustomerID = Int
type ReviewBody = String

-- We can introduce synonym for an existing type at any time, in order to give
-- a more descriptive name. For example, the string in our book review type
-- doesn't tell us what the string is for, we can clarify this 

data BookReview = BookReview BookInfo CustomerID ReviewBody

-- Type synonyms are purely for making code more readable (nothing more!)

type BookRecord = (BookInfo, BookReview)

-- We can use BookRecord as a synonym for the tuple (BookInfo, BookReview)

-- Algebraic data types: have more than one value constructor 
-- i.e. data Bool = False | True

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                  | CashOnDelivery
                  | Invoice CustomerID
                    deriving (Show)

-- If two algebraic data types have the same structure, but different names,
-- they are distinct! 

-- Record syntax, makes writing boilerplate easier.

data Customer = Customer {
      customerID      :: CustomerID,
      customerName    :: String,
      customerAddress :: Address
  } deriving (Show)

-- This is the same as the following:


-- file: ch03/AltCustomer.hs
-- data Customer = Customer Int String [String]
--                deriving (Show)
-- customerID :: Customer -> Int 
-- customerID (Customer id _ _) = id

-- customerName :: Customer -> String 
-- customerName (Customer _ name _) = name

-- customerAddress :: Customer -> [String] 
-- customerAddress (Customer _ _ address) = address

-- For each of the fields that we name in our type definition, Haskell creates
-- an accessor function of that name.

-- We can still use the usual application syntax to create a value of this type.
-- i.e.

customer1 = Customer 271828 "J.R. Hacker"
            ["255 Syntax Ct", 
             "Milpitas, CA 95135",
             "USA"]

-- or

customer2 = Customer {
              customerID = 271828,
              customerAddress = ["104875 Disk Drive",
                                 "Palo Alto, CA 94301",
                                 "USA"],
              customerName = "Jane Q. Citizen"
            }

--plus, we get customerName for free! etc. 

-- recursive types are the same
-- i.e.

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

simpleTree = Node "parent" (Node "left child" Empty Empty)
                           (Node "right child" Empty Empty)

-- Reporting errors
-- Haskell provides a standard function, error :: String -> a, that we can call
-- when something has gone terribly wrong in our code. We give it the error
-- message to display

mySecond :: [a] -> a
mySecond xs = if null (tail xs)
              then error "list too short"
              else head (tail xs)

-- a more controlled approach (maybe)

safeSecond :: [a] -> Maybe a
safeSecond [] = Nothing
safeSecond xs = if null (tail xs)
                then Nothing
                else Just (head (tail xs))

--------------------------------------------------------------------------------

-- ON INTRODUCING LOCAL VARIABLES

lend amount balance = let reserve    = 100
                          newBalance = balance - amount
                      in if (balance < reserve)
                          then Nothing
                          else Just newBalance

-- the keywords to look out for here are let, which starts a block of variable
-- delcarations, and in, which ends it. Each line troduces a new variable. The
-- name is on the left of the =, and the expresion to which it is bound is on
-- the right

-- the name is bound to an expression, not to a value. (remember... lazy 
-- evalution!!)

-- We can "nest" multiple let blocks inside eachother in an expression

foo = let a = 1
      in let b = 2
         in a + b

-- It's perfectly legal, but not exactly wise, to repeat a variable name in a
-- nested let expression:
bar = let x = 1
      in ((let x = "foo" in x), x)

-- here, the inner x is hiding, or shadowing, the outer x. It has the same name
-- but a different type in value

-- We can also shadow a function's parameters, leading to even stranger results

quux a = let a = "foo"
         in a ++ "eek"

-- because the function's argument a is never used in the body of the function,
-- due to being shadowed by the let bound a, the argument can have any type at
-- all quux :: t -> [Char] (always returns "fooeek")
      
-- we can use another mechanism to introduce local variables, the where clause.
-- The definitions in a where clause apply to the code that precedes it. 
-- We can redefine lend thusly:

lend2 amount balance = if (amount < reserve * 0.5)
                       then Just newBalance
                       else Nothing
      where reserve  = 100
            newBalance = balance - amount

-- can define things after we use them, which could be useful.

-- we can also define functions locally, just like variables

pluralise :: String -> [Int] -> [String]
pluralise word counts = map plural counts
          where plural 0 = "no " ++ word ++ "s"
                plural 1 = "one " ++ word ++ "s"
                plural n = show n ++ " " ++ word ++ "s"


-- We can also define variables, as well as functions, at the top level of a
-- source file
itemName = "Weighted Companion Cube"

-- In Haskell, whitespace has a meaning. Haskell uses indentation as a cue to
-- parse code. This use of layout to convey structure is sometimes called
-- the offside rule. At the beginning of a source file, the first top level
-- declaration or definition can start in any column, and the Haskell compiler
-- or interpreter remembers that indentation level. Every subsequent top-level
-- delcaration must have the same indentation

-- Function definitions are not the only place where we can use pattern matching
-- The case construct lets us mattern match within a construction

fromMaybe defval wrapped =
  case wrapped of
    Nothing    -> defval
    Just value -> value

-- A pattern can be followed by zero or more guards, each an expression of type
-- Bool. A guard is introduced by a | symbol. This is followed by the guard
-- expression, then an = symbol (or -> if we are in a case expression), then the
-- body to use if the guard expression evaluates to True

nodesAreSame (Node a _ _) (Node b _ _) | a == b = Just a
nodesAreSame _ _ = Nothing

lend3 amount balance
  | amount <= 0            = Nothing
  | amount > reserve * 0.5 = Nothing
  | otherwise              = Just newBalance
  where reserve    = 100
        newBalance = balance - amount

-- otherwise is simply a variable bound to the value true that aids in
-- readability
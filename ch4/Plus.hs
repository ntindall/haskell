-- p 76
import Data.List
-- Usually, when we define or apply a function in Haskell, we write the name of
-- the function, followed by its arguments. This notation is referred to as
-- prefix. 

-- If a function or constructor takes two or more arguments, we have the option 
-- of using infix form, where we place it between its first and second arguments

-- TO define or apply a function or value constructor using infix notation, we
-- enclose its name in backtick characters (sometimes known as backquotes).


a `plus` b = a + b

data a `Pair` b = a `Pair` b
                  deriving (Show)

foo = Pair 1 2
bar = True `Pair` "quux"

-- can help with readability

x = "foo" `isPrefixOf` "foobar"
y = "bar" `isSuffixOf` "foobar"


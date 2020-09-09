module Expression (
    UnaryOperator(..),
    BinaryOperator(..),
    Expression(..),
    Value(..),
    ExprV,
    Repr(..)
    ) where

data UnaryOperator = Minus deriving (Eq, Read, Show)
data BinaryOperator = Plus | Times deriving (Eq, Read, Show)

data Expression a = Leaf a
                  | UnaryOperation UnaryOperator (Expression a)
                  | BinaryOperation BinaryOperator (Expression a) (Expression a)
                  deriving (Eq, Read, Show)

data Value = Variable String
           | Constant Int 
           deriving (Eq, Read, Show)

type ExprV = Expression Value

class Repr a where
    repr :: a -> String

instance Repr UnaryOperator where
    repr Minus = "-"

instance Repr BinaryOperator where
    repr Plus = "+"
    repr Times = "*"

instance Repr Value where
    repr (Variable str) = str
    repr (Constant c) = show c
module Types where

data Type
  = Free
  | Int
  | Bool
  | Arrow Type Type
  deriving (Show, Eq)

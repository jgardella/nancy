module Types where

data Type
  = Int
  | Bool
  | Arrow Type Type
  deriving Eq

instance Show Type where
  show Int = "int"
  show Bool = "bool"
  show (Arrow l r) = show l ++ " -> " ++ show r

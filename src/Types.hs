module Types where

import qualified Proof as P

data Type
  = Int
  | Bool
  | Arrow Type Type
  | Trail
  | Box String P.Proof Type
  | Audited Type
  deriving (Eq, Show)

--instance Show Type where
--  show Int = "int"
--  show Bool = "bool"
--  show (Arrow l r) = show l ++ " -> " ++ show r

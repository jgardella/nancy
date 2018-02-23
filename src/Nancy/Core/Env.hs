module Nancy.Core.Env where

import qualified Data.Map as Map
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer

newtype Env v = Env (Map.Map String v)
  deriving (Eq, Show)

instance (Pretty v) => Pretty (Env v) where
  pPrint (Env m) =
    braces $ vcat prettyEnv
    where prettyEnv = Map.foldlWithKey (\a k v -> nest 2 (pPrint k <+> text "=>" <+> pPrint v):a) [] m

empty :: Env v
empty = Env Map.empty

save :: String -> v -> Env v -> Env v
save k v (Env m) = Env $ Map.insert k v m

load :: String -> Env v -> Maybe v
load k (Env m) = Map.lookup k m

loadE :: String -> e -> Env v -> ReaderT y (ExceptT e Identity) v
loadE s e (Env m) =
  case Map.lookup s m of
    (Just v) -> return v
    Nothing -> throwError e

loadES :: String -> e -> Env v -> ReaderT y (ExceptT e (WriterT [String] Identity)) v
loadES s e (Env m) =
  case Map.lookup s m of
    (Just v) -> return v
    Nothing -> throwError e

keys :: Env v -> [String]
keys (Env m) = Map.keys m

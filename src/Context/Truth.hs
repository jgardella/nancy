module Context.Truth where

import qualified Data.Map as Map

type TruthContext v = Map.Map String v

empty :: TruthContext v
empty = Map.empty

save :: String -> v -> TruthContext v -> TruthContext v
save = Map.insert

load :: String -> TruthContext v -> Maybe v
load = Map.lookup

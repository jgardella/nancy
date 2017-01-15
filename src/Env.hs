module Env where

import qualified Data.Map as Map

type Env v = Map.Map String v

empty :: Env v
empty = Map.empty

save :: String -> v -> Env v -> Env v
save = Map.insert

load :: String -> Env v -> Maybe v
load = Map.lookup

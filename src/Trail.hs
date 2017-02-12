module Trail where

import qualified Proof as P
import qualified Types as T

data Trail
    = Reflexivity P.Proof
    | Symmetry Trail
    | Transitivity Trail Trail
    | Beta T.Type P.Proof P.Proof
    | BetaBox T.Type P.Proof P.Proof
    | TrailInspection
    | AbsCompat T.Type Trail
    | AppCompat Trail Trail
    | LetCompat T.Type Trail Trail
    | ReplacementCompat [Trail]

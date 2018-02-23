module Nancy.Core.Util where

import Nancy.Core.Language as L
import Nancy.Core.Errors.Interpreter as Err
import Nancy.Core.Env
import qualified Data.Map as Map
import Data.List
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (== x) xs

witSubOverVar :: String -> L.Witness -> L.Witness -> L.Witness
witSubOverVar a w same@(L.VarWit var)
  | a == var = w
  | a /= var = same
witSubOverVar _ _ same@(L.BangWit _) = same
witSubOverVar a w otherWit =
  mapWitness id (witSubOverVar a w) otherWit

witSubOverAVarOnType :: String -> L.Witness -> L.Type -> L.Type
witSubOverAVarOnType u w =
  mapType (witSubOverAVarOnWit u w) (witSubOverAVarOnType u w)

witSubOverAVarOnWit :: String -> L.Witness -> L.Witness -> L.Witness
witSubOverAVarOnWit u w same@(L.AVarWit var)
  | u == var = w
  | u /= var = same
witSubOverAVarOnWit u w otherWit =
  mapWitness id (witSubOverAVarOnWit u w) otherWit

witSubOverAVarOnTrail u w =
  mapTrail
    (witSubOverAVarOnWit u w)
    (witSubOverAVarOnTrail u w)
    (witSubOverAVarOnType u w)

valueToExp :: L.Value -> L.Exp
valueToExp (L.IntVal n) = L.Number n
valueToExp (L.BoolVal b) = L.Boolean b
valueToExp (L.VarVal x) = L.Var x
valueToExp (L.AVarVal x) = L.AVar x
valueToExp (L.LamVal arg argType body) = L.Lam arg argType body
valueToExp (L.BangVal bodyVal trail) = L.Bang (valueToExp bodyVal) trail

valueSubOverVar :: L.Value -> String -> L.Exp -> L.Exp
valueSubOverVar value var same@(L.Var x)
  | x == var = valueToExp value
  | x /= var = same
valueSubOverVar value var same@L.Bang{} = same
valueSubOverVar value var otherExp =
  mapExp (valueSubOverVar value var) otherExp

termSubOverAVar :: L.Trail -> String -> L.Value -> L.Witness -> L.Exp -> L.Exp
termSubOverAVar trail var val _ same@(L.AVar u)
  | u == var = valueToExp val
  | u /= var = same
termSubOverAVar trail var val wit (L.Bang bangExp bangTrail) =
  L.Bang (termSubOverAVar trail var val wit bangExp) (witSubOverAVarOnTrail var wit bangTrail <--> trailSubOverAVar trail var val wit bangExp)
termSubOverAVar trail var val wit otherExp =
  mapExp (termSubOverAVar trail var val wit) otherExp

trailSubOverAVar :: L.Trail -> String -> L.Value -> L.Witness -> L.Exp -> L.Trail
trailSubOverAVar trail var _ _ exp@(L.AVar u)
  | u == var = trail
  | u /= var = L.RTrail $ getWit exp
trailSubOverAVar trail var val wit (L.Lam arg argType argExp) =
  L.LamTrail arg argType (trailSubOverAVar trail var val wit argExp)
trailSubOverAVar trail var val wit (L.App lamExp argExp) =
  L.AppTrail (trailSubOverAVar trail var val wit lamExp) (trailSubOverAVar trail var val wit argExp)
trailSubOverAVar trail var val wit (L.Bang bangExp bangTrail) =
  L.RTrail $ L.BangWit $ witSubOverAVarOnWit var wit (getSource bangTrail)
trailSubOverAVar trail var val wit (L.Let letVar letVarType argExp bodyExp) =
  L.LetTrail letVar letVarType (trailSubOverAVar trail var val wit argExp) (trailSubOverAVar trail var val wit bodyExp)
trailSubOverAVar trail var val wit (L.Inspect branches) =
  L.TrplTrail $ fmap (trailSubOverAVar trail var val wit) branches
trailSubOverAVar _ _ _ _ other =
  L.RTrail $ getWit other

getSource :: L.Trail -> L.Witness
getSource (L.RTrail witness) =
  witness
getSource (L.TTrail trl1 _) =
  getSource trl1
getSource (L.BaTrail arg argType bodyWit argWit) =
  L.AppWit (L.LamWit arg argType bodyWit) argWit
getSource (L.BbTrail arg argType argWit bodyWit) =
  L.LetWit arg argType (L.BangWit argWit) bodyWit
getSource (L.TiTrail _ branches) =
  L.TiWit branches
getSource (L.LamTrail arg argType bodyTrail) =
  L.LamWit arg argType (getSource bodyTrail)
getSource (L.AppTrail lamTrail argTrail) =
  L.AppWit (getSource lamTrail) (getSource argTrail)
getSource (L.LetTrail arg argType argTrail bodyTrail) =
  L.LetWit arg argType (getSource argTrail) (getSource bodyTrail)
getSource (L.TrplTrail branches) =
  L.TiWit $ fmap getSource branches

getTarget :: L.Trail -> L.Witness
getTarget (L.RTrail s) = s
getTarget (L.TTrail trail1 trail2) = getTarget trail2
getTarget (L.BaTrail var varType bodyWit argWit) =
  witSubOverVar var argWit bodyWit
getTarget (L.BbTrail var varType argWit bodyWit) =
  witSubOverAVarOnWit var argWit bodyWit
getTarget (L.TiTrail trail
    (TrailBranches
      rWit
      tWit
      baWit
      bbWit
      tiWit
      lamWit
      appWit
      letWit
      trplWit)) =
  undefined
getTarget (L.LamTrail var varType bodyTrail) =
  L.LamWit var varType (getTarget bodyTrail)
getTarget (L.AppTrail lamTrail argTrail) =
  L.AppWit (getTarget lamTrail) (getTarget argTrail)
getTarget (L.LetTrail var varType argTrail bodyTrail) =
  L.LetWit var varType (getTarget argTrail) (getTarget bodyTrail)
getTarget (L.TrplTrail branches) =
  L.TiWit $ fmap getTarget branches

getWit :: L.Exp -> L.Witness
getWit (L.Number n) =
  L.IntWit n
getWit (L.Boolean b) =
  L.BoolWit b
getWit (L.Brack exp) =
  getWit exp
getWit (L.Var x) =
  L.VarWit x
getWit (L.AVar u) =
  L.AVarWit u
getWit (L.Lam arg argType body) =
  L.LamWit arg argType (getWit body)
getWit (L.App lam arg) =
  L.AppWit (getWit lam) (getWit arg)
getWit (L.Bang exp trail) =
  getSource trail
getWit (L.Let u typ arg body) =
  L.LetWit u typ (getWit arg) (getWit body)
getWit (L.Inspect branches) =
  L.TiWit $ fmap getWit branches

foldTrailToTerm :: L.TrailBranches L.Exp -> L.Trail -> L.Exp
foldTrailToTerm L.TrailBranches{rB=exp} (L.RTrail _) =
  exp
foldTrailToTerm branches@L.TrailBranches{tB=exp} (L.TTrail t1 t2) =
  L.App
    (L.App
     exp
     (foldTrailToTerm branches t1))
    (foldTrailToTerm branches t2)
foldTrailToTerm L.TrailBranches{baB=exp} L.BaTrail{} =
  exp
foldTrailToTerm L.TrailBranches{bbB=exp} L.BbTrail{} =
  exp
foldTrailToTerm L.TrailBranches{tiB=exp} L.TiTrail{} =
  exp
foldTrailToTerm branches@L.TrailBranches{lamB=exp} (L.LamTrail _ _ bodyTrail) =
  L.App exp (foldTrailToTerm branches bodyTrail)
foldTrailToTerm branches@ L.TrailBranches{appB=exp} (L.AppTrail lamTrail bodyTrail) =
  L.App
    (L.App
     exp
     (foldTrailToTerm branches lamTrail))
    (foldTrailToTerm branches bodyTrail)
foldTrailToTerm branches@L.TrailBranches{letB=exp} (L.LetTrail _ _ argTrail bodyTrail) =
  L.App
    (L.App
     exp
     (foldTrailToTerm branches argTrail))
    (foldTrailToTerm branches bodyTrail)
foldTrailToTerm foldBranches@L.TrailBranches{trplB=exp}
  (L.TrplTrail trailBranches) =
    foldHelper exp (trailBranchesToList trailBranches)
  where
    foldHelper v [x] =
      L.App
        v
        (foldTrailToTerm foldBranches x)
    foldHelper v (x:xs) =
      L.App
        (foldHelper v xs)
        (foldTrailToTerm foldBranches x)

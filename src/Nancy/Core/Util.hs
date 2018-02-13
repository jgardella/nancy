module Nancy.Core.Util where

import qualified Nancy.Core.Language as L
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
witSubOverVar _ _ same@(L.IntWit _) = same
witSubOverVar _ _ same@(L.BoolWit _) = same
witSubOverVar a w same@(L.VarWit var)
  | a == var = w
  | a /= var = same
witSubOverVar _ _ same@(L.AVarWit _) = same
witSubOverVar a w (L.LamWit arg argType bodyWit) =
  L.LamWit arg argType (witSubOverVar a w bodyWit)
witSubOverVar a w (L.AppWit lamWit argWit) =
  L.AppWit (witSubOverVar a w lamWit) (witSubOverVar a w argWit)
witSubOverVar _ _ same@(L.BangWit _) = same
witSubOverVar a w (L.LetWit var varType argWit bodyWit) =
  L.LetWit var varType (witSubOverVar a w argWit) (witSubOverVar a w bodyWit)
witSubOverVar a w (L.TiWit branches) =
  L.TiWit $ fmap (witSubOverVar a w) branches

witSubOverAVarOnType :: String -> L.Witness -> L.Type -> L.Type
witSubOverAVarOnType _ _ same@L.IntType = same
witSubOverAVarOnType _ _ same@L.BoolType = same
witSubOverAVarOnType u w (L.LamType argType bodyType) =
  L.LamType (witSubOverAVarOnType u w argType) (witSubOverAVarOnType u w bodyType)
witSubOverAVarOnType u w (L.BangType bangType bangWit) =
  L.BangType (witSubOverAVarOnType u w bangType) (witSubOverAVarOnWit u w bangWit)

witSubOverAVarOnWit :: String -> L.Witness -> L.Witness -> L.Witness
witSubOverAVarOnWit _ _ same@(L.VarWit _) = same
witSubOverAVarOnWit u w same@(L.AVarWit var)
  | u == var = w
  | u /= var = same
witSubOverAVarOnWit _ _ same@(L.IntWit _) = same
witSubOverAVarOnWit _ _ same@(L.BoolWit _) = same
witSubOverAVarOnWit u w (L.LamWit arg argType bodyWit) =
  L.LamWit arg argType (witSubOverAVarOnWit u w bodyWit)
witSubOverAVarOnWit u w (L.AppWit leftWit rightWit) =
  L.AppWit (witSubOverAVarOnWit u w leftWit) (witSubOverAVarOnWit u w rightWit)
witSubOverAVarOnWit u w (L.BangWit bangWit) =
  L.BangWit (witSubOverAVarOnWit u w bangWit)
witSubOverAVarOnWit u w (L.LetWit var varType argWit bodyWit) =
  L.LetWit var varType (witSubOverAVarOnWit u w argWit) (witSubOverAVarOnWit u w bodyWit)
witSubOverAVarOnWit u w (L.TiWit branches) =
  L.TiWit $ fmap (witSubOverAVarOnWit u w) branches

valueToExp :: L.Value -> L.Exp
valueToExp (L.IntVal n) = L.Number n
valueToExp (L.BoolVal b) = L.Boolean b
valueToExp (L.VarVal x) = L.Var x
valueToExp (L.AVarVal x) = L.AVar x
valueToExp (L.LamVal arg argType body) = L.Lam arg argType body
valueToExp (L.BangVal bodyVal trail) = L.Bang (valueToExp bodyVal) trail

valueSubOverVar :: L.Value -> String -> L.Exp -> L.Exp
valueSubOverVar _ _  same@(L.Number _) = same
valueSubOverVar _ _ same@(L.Boolean _) = same
valueSubOverVar value var (L.Brack exp) = L.Brack (valueSubOverVar value var exp)
valueSubOverVar value var same@(L.Var x)
  | x == var = valueToExp value
  | x /= var = same
valueSubOverVar _ _ same@(L.AVar _) = same
valueSubOverVar value var (L.Lam arg argType body) =
  L.Lam arg argType (valueSubOverVar value var body)
valueSubOverVar value var (L.App lam arg) =
  L.App (valueSubOverVar value var lam) (valueSubOverVar value var arg)
valueSubOverVar value var same@L.Bang{} = same
valueSubOverVar value var (L.Let letVar letVarType arg body) =
  L.Let letVar letVarType (valueSubOverVar value var arg) (valueSubOverVar value var body)
valueSubOverVar value var (L.Inspect branches) =
  L.Inspect $ fmap (valueSubOverVar value var) branches

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
getTarget (L.TiTrail trail branches) =
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

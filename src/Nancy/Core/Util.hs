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

witSubOnType :: String -> L.Witness -> L.Type -> L.Type
witSubOnType _ _ same@L.IntType = same
witSubOnType _ _ same@L.BoolType = same
witSubOnType u w (L.LamType argType bodyType) =
  L.LamType (witSubOnType u w argType) (witSubOnType u w bodyType)
witSubOnType u w (L.BangType bangType bangWit) =
  L.BangType (witSubOnType u w bangType) (witSubOnWit u w bangWit)

witSubOnWit :: String -> L.Witness -> L.Witness -> L.Witness
witSubOnWit _ _ same@(L.VarWit _) = same
witSubOnWit _ _ same@(L.IntWit _) = same
witSubOnWit _ _ same@(L.BoolWit _) = same
witSubOnWit u w (L.LamWit arg argType bodyWit) =
  L.LamWit arg argType (witSubOnWit u w bodyWit)
witSubOnWit u w (L.AppWit leftWit rightWit) =
  L.AppWit (witSubOnWit u w leftWit) (witSubOnWit u w rightWit)
witSubOnWit u w same@(L.AVarWit var)
  | u == var = w
  | u /= var = same
witSubOnWit u w (L.BangWit bangWit) =
  L.BangWit (witSubOnWit u w bangWit)
witSubOnWit u w (L.LetWit var varType argWit bodyWit) =
  L.LetWit var varType (witSubOnWit u w argWit) (witSubOnWit u w bodyWit)
witSubOnWit u w (L.TiWit rWit tWit baWit bbWit tiWit lamWit appWit letWit trplWit) =
  L.TiWit
    (witSubOnWit u w rWit)
    (witSubOnWit u w tWit)
    (witSubOnWit u w baWit)
    (witSubOnWit u w bbWit)
    (witSubOnWit u w tiWit)
    (witSubOnWit u w lamWit)
    (witSubOnWit u w appWit)
    (witSubOnWit u w letWit)
    (witSubOnWit u w trplWit)

getSource :: L.Trail -> L.Witness
getSource (L.RTrail witness) =
  witness
getSource (L.TTrail trl1 _) =
  getSource trl1
getSource (L.BaTrail arg argType bodyWit argWit) =
  L.AppWit (L.LamWit arg argType bodyWit) argWit
getSource (L.BbTrail arg argType argWit bodyWit) =
  L.LetWit arg argType (L.BangWit argWit) bodyWit
getSource (L.TiTrail _ rWit tWit baWit bbWit tiWit lamWit appWit letWit trplWit) =
  L.TiWit rWit tWit baWit bbWit tiWit lamWit appWit letWit trplWit
getSource (L.LamTrail arg argType bodyTrail) =
  L.LamWit arg argType (getSource bodyTrail)
getSource (L.AppTrail lamTrail argTrail) =
  L.AppWit (getSource lamTrail) (getSource argTrail)
getSource (L.LetTrail arg argType argTrail bodyTrail) =
  L.LetWit arg argType (getSource argTrail) (getSource bodyTrail)
getSource (L.TrplTrail
    rTrail
    tTrail
    baTrail
    bbTrail
    tiTrail
    absTrail
    appTrail
    letTrail
    trplTrail) =
  L.TiWit
    (getSource rTrail)
    (getSource tTrail)
    (getSource baTrail)
    (getSource bbTrail)
    (getSource tiTrail)
    (getSource absTrail)
    (getSource appTrail)
    (getSource letTrail)
    (getSource trplTrail)

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
getWit
  (L.Inspect
    rExp
    tExp
    baExp
    bbExp
    tiExp
    lamExp
    appExp
    letExp
    trplExp)
    =
  L.TiWit
    (getWit rExp)
    (getWit tExp)
    (getWit baExp)
    (getWit bbExp)
    (getWit tiExp)
    (getWit lamExp)
    (getWit appExp)
    (getWit letExp)
    (getWit trplExp)

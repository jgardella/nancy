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

varSubOnWit :: String -> L.Witness -> L.Witness -> L.Witness
varSubOnWit a w same@(L.VarWit var)
  | a == var = w
  | a /= var = same
varSubOnWit _ _ same@(L.AVarWit _) = same
varSubOnWit _ _ same@(L.IntWit _) = same
varSubOnWit _ _ same@(L.BoolWit _) = same
varSubOnWit a w (L.LamWit arg argType bodyWit) =
  L.LamWit arg argType (varSubOnWit a w bodyWit)
varSubOnWit a w (L.AppWit lamWit argWit) =
  L.AppWit (varSubOnWit a w lamWit) (varSubOnWit a w argWit)
varSubOnWit _ _ same@(L.BangWit _) = same
varSubOnWit a w (L.LetWit var varType argWit bodyWit) =
  L.LetWit var varType (varSubOnWit a w argWit) (varSubOnWit a w bodyWit)
varSubOnWit a w (L.TiWit rWit tWit baWit bbWit tiWit lamWit appWit letWit trplWit) =
  L.TiWit
    (varSubOnWit a w rWit)
    (varSubOnWit a w tWit)
    (varSubOnWit a w baWit)
    (varSubOnWit a w bbWit)
    (varSubOnWit a w tiWit)
    (varSubOnWit a w lamWit)
    (varSubOnWit a w appWit)
    (varSubOnWit a w letWit)
    (varSubOnWit a w trplWit)

avarSubOnType :: String -> L.Witness -> L.Type -> L.Type
avarSubOnType _ _ same@L.IntType = same
avarSubOnType _ _ same@L.BoolType = same
avarSubOnType u w (L.LamType argType bodyType) =
  L.LamType (avarSubOnType u w argType) (avarSubOnType u w bodyType)
avarSubOnType u w (L.BangType bangType bangWit) =
  L.BangType (avarSubOnType u w bangType) (avarSubOnWit u w bangWit)

avarSubOnWit :: String -> L.Witness -> L.Witness -> L.Witness
avarSubOnWit _ _ same@(L.VarWit _) = same
avarSubOnWit u w same@(L.AVarWit var)
  | u == var = w
  | u /= var = same
avarSubOnWit _ _ same@(L.IntWit _) = same
avarSubOnWit _ _ same@(L.BoolWit _) = same
avarSubOnWit u w (L.LamWit arg argType bodyWit) =
  L.LamWit arg argType (avarSubOnWit u w bodyWit)
avarSubOnWit u w (L.AppWit leftWit rightWit) =
  L.AppWit (avarSubOnWit u w leftWit) (avarSubOnWit u w rightWit)
avarSubOnWit u w (L.BangWit bangWit) =
  L.BangWit (avarSubOnWit u w bangWit)
avarSubOnWit u w (L.LetWit var varType argWit bodyWit) =
  L.LetWit var varType (avarSubOnWit u w argWit) (avarSubOnWit u w bodyWit)
avarSubOnWit u w (L.TiWit rWit tWit baWit bbWit tiWit lamWit appWit letWit trplWit) =
  L.TiWit
    (avarSubOnWit u w rWit)
    (avarSubOnWit u w tWit)
    (avarSubOnWit u w baWit)
    (avarSubOnWit u w bbWit)
    (avarSubOnWit u w tiWit)
    (avarSubOnWit u w lamWit)
    (avarSubOnWit u w appWit)
    (avarSubOnWit u w letWit)
    (avarSubOnWit u w trplWit)

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

getTarget :: L.Trail -> L.Witness
getTarget (L.RTrail s) = s
getTarget (L.TTrail trail1 trail2) = getTarget trail2
getTarget (L.BaTrail var varType bodyWit argWit) =
  varSubOnWit var argWit bodyWit
getTarget (L.BbTrail var varType argWit bodyWit) =
  avarSubOnWit var argWit bodyWit
getTarget (L.TiTrail trail rWit tWit baWit bbWit tiWit lamWit appWit letWit trplWit) =
  undefined
getTarget (L.LamTrail var varType bodyTrail) =
  L.LamWit var varType (getTarget bodyTrail)
getTarget (L.AppTrail lamTrail argTrail) =
  L.AppWit (getTarget lamTrail) (getTarget argTrail)
getTarget (L.LetTrail var varType argTrail bodyTrail) =
  L.LetWit var varType (getTarget argTrail) (getTarget bodyTrail)
getTarget (L.TrplTrail
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
    (getTarget rTrail)
    (getTarget tTrail)
    (getTarget baTrail)
    (getTarget bbTrail)
    (getTarget tiTrail)
    (getTarget absTrail)
    (getTarget appTrail)
    (getTarget letTrail)
    (getTarget trplTrail)

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

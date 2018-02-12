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
witSubOnType u w (L.ArrowType argType bodyType) =
  L.ArrowType (witSubOnType u w argType) (witSubOnType u w bodyType)
witSubOnType u w (L.BoxType boxWit boxType) =
  L.BoxType (witSubOnWit u w boxWit) (witSubOnType u w boxType)

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

getWit :: L.Exp -> ReaderT L.InterpretEnv (ExceptT InterpreterE (WriterT [String] (StateT L.Trail Identity))) L.Witness
getWit (L.Number n) =
  return $ L.IntWit n
getWit (L.Boolean b) =
  return $ L.BoolWit b
getWit (L.Brack exp) =
  getWit exp
getWit (L.Var x) =
  return $ L.VarWit x
getWit (L.Lam arg argType body) = do
  bodyWit <- getWit body
  return $ L.LamWit arg argType bodyWit
getWit (L.App lam arg) = do
  lamWit <- getWit lam
  argWit <- getWit arg
  return $ L.AppWit lamWit argWit
getWit (L.AVar u) =
  return $ L.AVarWit u
getWit (L.Bang exp) =
  undefined
  -- TODO
getWit (L.Let u typ arg body) = do
  argWit <- getWit arg
  bodyWit <- getWit body
  return $ L.LetWit u typ argWit bodyWit
getWit
  (L.Inspect
    (L.ReflexivityM exp_r)
    (L.TransitivityM t1 t2 exp_t)
    (L.BetaM exp_ba)
    (L.BetaBoxM exp_bb)
    (L.TrailInspectionM exp_ti)
    (L.AbstractionM abs1 exp_abs)
    (L.ApplicationM app1 app2 exp_app)
    (L.LetM let1 let2 exp_let)
    (L.TrplM r t ba bb ti lam app letArg trpl exp_trpl))
    = do
  rWit <- getWit exp_r
  tWit <- getWit exp_t
  baWit <- getWit exp_ba
  bbWit <- getWit exp_bb
  tiWit <- getWit exp_ti
  absWit <- getWit exp_abs
  appWit <- getWit exp_app
  letWit <- getWit exp_let
  trplWit <- getWit exp_trpl
  return $ L.TiWit
    rWit
    tWit
    baWit
    bbWit
    tiWit
    absWit
    appWit
    letWit
    trplWit

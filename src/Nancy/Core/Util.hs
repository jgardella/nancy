module Nancy.Core.Util where

import Nancy.Core.Language as L

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (== x) xs

witSubOverVar :: String -> L.Witness -> L.Witness -> L.Witness
witSubOverVar a w same@(L.Var var)
  | a == var = w
  | a /= var = same
witSubOverVar _ _ same@(L.Bang _ _) = same
witSubOverVar a w otherWit =
  mapExpr (witSubOverVar a w) otherWit

witSubOverAVarOnType :: String -> L.Witness -> L.Type -> L.Type
witSubOverAVarOnType u w =
  mapType (witSubOverAVarOnWit u w) (witSubOverAVarOnType u w)

witSubOverAVarOnWit :: String -> L.Witness -> L.Witness -> L.Witness
witSubOverAVarOnWit u w same@(L.AVar var)
  | u == var = w
  | u /= var = same
witSubOverAVarOnWit u w otherWit =
  mapExpr (witSubOverAVarOnWit u w) otherWit

witSubOverAVarOnTrail :: String -> L.Witness -> L.Trail -> L.Trail
witSubOverAVarOnTrail u w =
  mapTrail
    (witSubOverAVarOnWit u w)
    (witSubOverAVarOnTrail u w)
    (witSubOverAVarOnType u w)

valueToTerm :: L.Value -> L.Term
valueToTerm (L.IntVal n) = L.Number n
valueToTerm (L.BoolVal b) = L.Boolean b
valueToTerm (L.VarVal x) = L.Var x
valueToTerm (L.AVarVal x) = L.AVar x
valueToTerm (L.LamVal arg argType body) = L.Lam arg argType body
valueToTerm (L.BangVal bodyVal (L.TrailWithMode (_, trail))) = L.Bang (valueToTerm bodyVal) trail

valueSubOverVar :: L.Value -> String -> L.Term -> L.Term
valueSubOverVar value var same@(L.Var x)
  | x == var = valueToTerm value
  | x /= var = same
valueSubOverVar _ _ same@L.Bang{} = same
valueSubOverVar value var otherExpr =
  mapExpr (valueSubOverVar value var) otherExpr

termSubOverAVar :: L.Trail -> String -> L.Value -> L.Witness -> L.Term -> L.Term
termSubOverAVar _ var val _ same@(L.AVar u)
  | u == var = valueToTerm val
  | u /= var = same
termSubOverAVar trail var val wit (L.Bang bangExpr bangTrail) =
  L.Bang (termSubOverAVar trail var val wit bangExpr) (witSubOverAVarOnTrail var wit bangTrail <--> trailSubOverAVar trail var val wit bangExpr)
termSubOverAVar trail var val wit otherExpr =
  mapExpr (termSubOverAVar trail var val wit) otherExpr

trailSubOverAVar :: L.Trail -> String -> L.Value -> L.Witness -> L.Term -> L.Trail
trailSubOverAVar trail var _ _ expr@(L.AVar u)
  | u == var = trail
  | u /= var = L.RTrail $ getWit expr
trailSubOverAVar trail var val wit (L.Lam arg argType argExpr) =
  L.LamTrail arg argType (trailSubOverAVar trail var val wit argExpr)
trailSubOverAVar trail var val wit (L.App lamExpr argExpr) =
  L.AppTrail (trailSubOverAVar trail var val wit lamExpr) (trailSubOverAVar trail var val wit argExpr)
trailSubOverAVar trail var val wit (L.Plus leftExpr rightExpr) =
  L.PlusTrail (trailSubOverAVar trail var val wit leftExpr) (trailSubOverAVar trail var val wit rightExpr)
trailSubOverAVar trail var val wit (L.Eq leftExpr rightExpr) =
  L.EqTrail (trailSubOverAVar trail var val wit leftExpr) (trailSubOverAVar trail var val wit rightExpr)
trailSubOverAVar trail var val wit (L.Ite condExpr thenExpr elseExpr) =
  L.IteTrail
    (trailSubOverAVar trail var val wit condExpr)
    (trailSubOverAVar trail var val wit thenExpr)
    (trailSubOverAVar trail var val wit elseExpr)
trailSubOverAVar _ var _ wit (L.Bang _ bangTrail) =
  L.RTrail $ L.Bang (witSubOverAVarOnWit var wit (getSource bangTrail)) L.NoTrail
trailSubOverAVar trail var val wit (L.ALet letVar letVarType argExpr bodyExpr) =
  L.ALetTrail letVar letVarType (trailSubOverAVar trail var val wit argExpr) (trailSubOverAVar trail var val wit bodyExpr)
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
  L.App (L.Lam arg argType bodyWit) argWit
getSource (L.BbTrail arg argType argWit bodyWit) =
  L.ALet arg argType (L.Bang argWit L.NoTrail) bodyWit
getSource (L.TiTrail _ branches) =
  L.Inspect branches
getSource (L.LamTrail arg argType bodyTrail) =
  L.Lam arg argType (getSource bodyTrail)
getSource (L.AppTrail lamTrail argTrail) =
  L.App (getSource lamTrail) (getSource argTrail)
getSource (L.PlusTrail leftTrail rightTrail) =
  L.Plus (getSource leftTrail) (getSource rightTrail)
getSource (L.EqTrail leftTrail rightTrail) =
  L.Eq (getSource leftTrail) (getSource rightTrail)
getSource (L.IteTrail condTrail thenTrail elseTrail) =
  L.Ite
    (getSource condTrail)
    (getSource thenTrail)
    (getSource elseTrail)
getSource (L.ALetTrail arg argType argTrail bodyTrail) =
  L.ALet arg argType (getSource argTrail) (getSource bodyTrail)
getSource (L.TrplTrail branches) =
  L.Inspect $ fmap getSource branches

getTarget :: L.Trail -> L.Witness
getTarget (L.RTrail s) = s
getTarget (L.TTrail _ trail2) = getTarget trail2
getTarget (L.BaTrail var _ bodyWit argWit) =
  witSubOverVar var argWit bodyWit
getTarget (L.BbTrail var _ argWit bodyWit) =
  witSubOverAVarOnWit var argWit bodyWit
getTarget (L.TiTrail trail branches) =
  foldTrailToExpr branches trail
getTarget (L.LamTrail var varType bodyTrail) =
  L.Lam var varType (getTarget bodyTrail)
getTarget (L.AppTrail lamTrail argTrail) =
  L.App (getTarget lamTrail) (getTarget argTrail)
getTarget (L.PlusTrail leftTrail rightTrail) =
  L.Plus (getTarget leftTrail) (getTarget rightTrail)
getTarget (L.EqTrail leftTrail rightTrail) =
  L.Eq (getTarget leftTrail) (getTarget rightTrail)
getTarget (L.IteTrail condTrail thenTrail elseTrail) =
  L.Ite
    (getTarget condTrail)
    (getTarget thenTrail)
    (getTarget elseTrail)
getTarget (L.ALetTrail var varType argTrail bodyTrail) =
  L.ALet var varType (getTarget argTrail) (getTarget bodyTrail)
getTarget (L.TrplTrail branches) =
  L.Inspect $ fmap getTarget branches

getWit :: L.Term -> L.Witness
getWit (L.Number n) =
  L.Number n
getWit (L.Boolean b) =
  L.Boolean b
getWit (L.Brack expr) =
  L.Brack (getWit expr)
getWit (L.Var x) =
  L.Var x
getWit (L.AVar u) =
  L.AVar u
getWit (L.Lam arg argType body) =
  L.Lam arg argType (getWit body)
getWit (L.App lam arg) =
  L.App (getWit lam) (getWit arg)
getWit (L.Plus leftExpr rightExpr) =
  L.Plus (getWit leftExpr) (getWit rightExpr)
getWit (L.Eq leftExpr rightExpr) =
  L.Eq (getWit leftExpr) (getWit rightExpr)
getWit (L.Ite condExpr thenExpr elseExpr) =
  L.Ite
    (getWit condExpr)
    (getWit thenExpr)
    (getWit elseExpr)
getWit (L.Bang _ trail) =
  L.Bang (getSource trail) L.NoTrail
getWit (L.ALet u typ arg body) =
  L.ALet u typ (getWit arg) (getWit body)
getWit (L.Inspect branches) =
  L.Inspect $ fmap getWit branches

foldTrailToExpr :: L.TrailBranches (L.Expr a) -> L.Trail -> L.Expr a
foldTrailToExpr L.TrailBranches{r=expr} (L.RTrail _) =
  expr
foldTrailToExpr branches@L.TrailBranches{t=expr} (L.TTrail t1 t2) =
  L.App
    (L.App
     expr
     (foldTrailToExpr branches t1))
    (foldTrailToExpr branches t2)
foldTrailToExpr L.TrailBranches{ba=expr} L.BaTrail{} =
  expr
foldTrailToExpr L.TrailBranches{bb=expr} L.BbTrail{} =
  expr
foldTrailToExpr L.TrailBranches{ti=expr} L.TiTrail{} =
  expr
foldTrailToExpr branches@L.TrailBranches{lam=expr} (L.LamTrail _ _ bodyTrail) =
  L.App expr (foldTrailToExpr branches bodyTrail)
foldTrailToExpr branches@L.TrailBranches{app=expr} (L.AppTrail lamTrail bodyTrail) =
  L.App
    (L.App
     expr
     (foldTrailToExpr branches lamTrail))
    (foldTrailToExpr branches bodyTrail)
foldTrailToExpr branches@L.TrailBranches{plus=expr} (L.PlusTrail leftExpr rightExpr) =
  L.App
    (L.App
     expr
     (foldTrailToExpr branches leftExpr))
    (foldTrailToExpr branches rightExpr)
foldTrailToExpr branches@L.TrailBranches{eq=expr} (L.EqTrail leftExpr rightExpr) =
  L.App
    (L.App
     expr
     (foldTrailToExpr branches leftExpr))
    (foldTrailToExpr branches rightExpr)
foldTrailToExpr branches@L.TrailBranches{ite=expr} (L.IteTrail condTrail thenTrail elseTrail) =
  L.App
    (L.App
      (L.App
       expr
       (foldTrailToExpr branches condTrail))
      (foldTrailToExpr branches thenTrail))
    (foldTrailToExpr branches elseTrail)
foldTrailToExpr branches@L.TrailBranches{alet=expr} (L.ALetTrail _ _ argTrail bodyTrail) =
  L.App
    (L.App
     expr
     (foldTrailToExpr branches argTrail))
    (foldTrailToExpr branches bodyTrail)
foldTrailToExpr foldBranches@L.TrailBranches{trpl=expr}
  (L.TrplTrail trailBranches) =
    foldHelper expr (trailBranchesToList trailBranches)
  where
    foldHelper v [] = v
    foldHelper v [x] =
      L.App
        v
        (foldTrailToExpr foldBranches x)
    foldHelper v (x:xs) =
      L.App
        (foldHelper v xs)
        (foldTrailToExpr foldBranches x)

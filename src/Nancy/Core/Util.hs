module Nancy.Core.Util where

import Nancy.Core.Language as L

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

witSubOverAVarOnTrail :: String -> L.Witness -> L.Trail -> L.Trail
witSubOverAVarOnTrail u w =
  mapTrail
    (witSubOverAVarOnWit u w)
    (witSubOverAVarOnTrail u w)
    (witSubOverAVarOnType u w)

valueToExpr :: L.Value -> L.Expr
valueToExpr (L.IntVal n) = L.Number n
valueToExpr (L.BoolVal b) = L.Boolean b
valueToExpr (L.VarVal x) = L.Var x
valueToExpr (L.AVarVal x) = L.AVar x
valueToExpr (L.LamVal arg argType body) = L.Lam arg argType body
valueToExpr (L.BangVal bodyVal trail) = L.Bang (valueToExpr bodyVal) trail

valueSubOverVar :: L.Value -> String -> L.Expr -> L.Expr
valueSubOverVar value var same@(L.Var x)
  | x == var = valueToExpr value
  | x /= var = same
valueSubOverVar _ _ same@L.Bang{} = same
valueSubOverVar value var otherExpr =
  mapExpr (valueSubOverVar value var) otherExpr

termSubOverAVar :: L.Trail -> String -> L.Value -> L.Witness -> L.Expr -> L.Expr
termSubOverAVar _ var val _ same@(L.AVar u)
  | u == var = valueToExpr val
  | u /= var = same
termSubOverAVar trail var val wit (L.Bang bangExpr bangTrail) =
  L.Bang (termSubOverAVar trail var val wit bangExpr) (witSubOverAVarOnTrail var wit bangTrail <--> trailSubOverAVar trail var val wit bangExpr)
termSubOverAVar trail var val wit otherExpr =
  mapExpr (termSubOverAVar trail var val wit) otherExpr

trailSubOverAVar :: L.Trail -> String -> L.Value -> L.Witness -> L.Expr -> L.Trail
trailSubOverAVar trail var _ _ expr@(L.AVar u)
  | u == var = trail
  | u /= var = L.RTrail $ getWit expr
trailSubOverAVar trail var val wit (L.Lam arg argType argExpr) =
  L.LamTrail arg argType (trailSubOverAVar trail var val wit argExpr)
trailSubOverAVar trail var val wit (L.App lamExpr argExpr) =
  L.AppTrail (trailSubOverAVar trail var val wit lamExpr) (trailSubOverAVar trail var val wit argExpr)
trailSubOverAVar trail var val wit (L.Plus leftExpr rightExpr) =
  L.PlusTrail (trailSubOverAVar trail var val wit leftExpr) (trailSubOverAVar trail var val wit rightExpr)
trailSubOverAVar _ var _ wit (L.Bang _ bangTrail) =
  L.RTrail $ L.BangWit $ witSubOverAVarOnWit var wit (getSource bangTrail)
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
  L.AppWit (L.LamWit arg argType bodyWit) argWit
getSource (L.BbTrail arg argType argWit bodyWit) =
  L.ALetWit arg argType (L.BangWit argWit) bodyWit
getSource (L.TiTrail _ branches) =
  L.TiWit branches
getSource (L.LamTrail arg argType bodyTrail) =
  L.LamWit arg argType (getSource bodyTrail)
getSource (L.AppTrail lamTrail argTrail) =
  L.AppWit (getSource lamTrail) (getSource argTrail)
getSource (L.PlusTrail leftTrail rightTrail) =
  L.PlusWit (getSource leftTrail) (getSource rightTrail)
getSource (L.ALetTrail arg argType argTrail bodyTrail) =
  L.ALetWit arg argType (getSource argTrail) (getSource bodyTrail)
getSource (L.TrplTrail branches) =
  L.TiWit $ fmap getSource branches

getTarget :: L.Trail -> L.Witness
getTarget (L.RTrail s) = s
getTarget (L.TTrail _ trail2) = getTarget trail2
getTarget (L.BaTrail var _ bodyWit argWit) =
  witSubOverVar var argWit bodyWit
getTarget (L.BbTrail var _ argWit bodyWit) =
  witSubOverAVarOnWit var argWit bodyWit
getTarget (L.TiTrail trail branches) =
  foldTrailToWit branches trail
getTarget (L.LamTrail var varType bodyTrail) =
  L.LamWit var varType (getTarget bodyTrail)
getTarget (L.AppTrail lamTrail argTrail) =
  L.AppWit (getTarget lamTrail) (getTarget argTrail)
getTarget (L.PlusTrail leftTrail rightTrail) =
  L.PlusWit (getTarget leftTrail) (getTarget rightTrail)
getTarget (L.ALetTrail var varType argTrail bodyTrail) =
  L.ALetWit var varType (getTarget argTrail) (getTarget bodyTrail)
getTarget (L.TrplTrail branches) =
  L.TiWit $ fmap getTarget branches

getWit :: L.Expr -> L.Witness
getWit (L.Number n) =
  L.IntWit n
getWit (L.Boolean b) =
  L.BoolWit b
getWit (L.Brack expr) =
  getWit expr
getWit (L.Var x) =
  L.VarWit x
getWit (L.AVar u) =
  L.AVarWit u
getWit (L.Lam arg argType body) =
  L.LamWit arg argType (getWit body)
getWit (L.App lam arg) =
  L.AppWit (getWit lam) (getWit arg)
getWit (L.Plus leftExpr rightExpr) =
  L.PlusWit (getWit leftExpr) (getWit rightExpr)
getWit (L.Bang _ trail) =
  L.BangWit (getSource trail)
getWit (L.ALet u typ arg body) =
  L.ALetWit u typ (getWit arg) (getWit body)
getWit (L.Inspect branches) =
  L.TiWit $ fmap getWit branches

foldTrailToTerm :: L.TrailBranches L.Expr -> L.Trail -> L.Expr
foldTrailToTerm L.TrailBranches{r=expr} (L.RTrail _) =
  expr
foldTrailToTerm branches@L.TrailBranches{t=expr} (L.TTrail t1 t2) =
  L.App
    (L.App
     expr
     (foldTrailToTerm branches t1))
    (foldTrailToTerm branches t2)
foldTrailToTerm L.TrailBranches{ba=expr} L.BaTrail{} =
  expr
foldTrailToTerm L.TrailBranches{bb=expr} L.BbTrail{} =
  expr
foldTrailToTerm L.TrailBranches{ti=expr} L.TiTrail{} =
  expr
foldTrailToTerm branches@L.TrailBranches{lam=expr} (L.LamTrail _ _ bodyTrail) =
  L.App expr (foldTrailToTerm branches bodyTrail)
foldTrailToTerm branches@ L.TrailBranches{app=expr} (L.AppTrail lamTrail bodyTrail) =
  L.App
    (L.App
     expr
     (foldTrailToTerm branches lamTrail))
    (foldTrailToTerm branches bodyTrail)
foldTrailToTerm branches@ L.TrailBranches{app=expr} (L.PlusTrail leftExpr rightExpr) =
  L.App
    (L.App
     expr
     (foldTrailToTerm branches leftExpr))
    (foldTrailToTerm branches rightExpr)
foldTrailToTerm branches@L.TrailBranches{alet=expr} (L.ALetTrail _ _ argTrail bodyTrail) =
  L.App
    (L.App
     expr
     (foldTrailToTerm branches argTrail))
    (foldTrailToTerm branches bodyTrail)
foldTrailToTerm foldBranches@L.TrailBranches{trpl=expr}
  (L.TrplTrail trailBranches) =
    foldHelper expr (trailBranchesToList trailBranches)
  where
    foldHelper v [] = v
    foldHelper v [x] =
      L.App
        v
        (foldTrailToTerm foldBranches x)
    foldHelper v (x:xs) =
      L.App
        (foldHelper v xs)
        (foldTrailToTerm foldBranches x)

foldTrailToWit :: L.TrailBranches L.Witness -> L.Trail -> L.Witness
foldTrailToWit L.TrailBranches{r=wit} (L.RTrail _) =
  wit
foldTrailToWit branches@L.TrailBranches{t=wit} (L.TTrail t1 t2) =
  L.AppWit
    (L.AppWit
     wit
     (foldTrailToWit branches t1))
    (foldTrailToWit branches t2)
foldTrailToWit L.TrailBranches{ba=wit} L.BaTrail{} =
  wit
foldTrailToWit L.TrailBranches{bb=wit} L.BbTrail{} =
  wit
foldTrailToWit L.TrailBranches{ti=wit} L.TiTrail{} =
  wit
foldTrailToWit branches@L.TrailBranches{lam=wit} (L.LamTrail _ _ bodyTrail) =
  L.AppWit wit (foldTrailToWit branches bodyTrail)
foldTrailToWit branches@ L.TrailBranches{app=wit} (L.AppTrail lamTrail bodyTrail) =
  L.AppWit
    (L.AppWit
     wit
     (foldTrailToWit branches lamTrail))
    (foldTrailToWit branches bodyTrail)
foldTrailToWit branches@ L.TrailBranches{app=wit} (L.PlusTrail leftTrail rightTrail) =
  L.AppWit
    (L.AppWit
     wit
     (foldTrailToWit branches leftTrail))
    (foldTrailToWit branches rightTrail)
foldTrailToWit branches@L.TrailBranches{alet=wit} (L.ALetTrail _ _ argTrail bodyTrail) =
  L.AppWit
    (L.AppWit
     wit
     (foldTrailToWit branches argTrail))
    (foldTrailToWit branches bodyTrail)
foldTrailToWit foldBranches@L.TrailBranches{trpl=wit}
  (L.TrplTrail trailBranches) =
    foldHelper wit (trailBranchesToList trailBranches)
  where
    foldHelper v [] = v
    foldHelper v [x] =
      L.AppWit
        v
        (foldTrailToWit foldBranches x)
    foldHelper v (x:xs) =
      L.AppWit
        (foldHelper v xs)
        (foldTrailToWit foldBranches x)

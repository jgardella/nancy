inspect {
  r -> 0
  t -> fun (t1:int) (t2:int) ->
    t1 + t2
  ba -> 1
  bb -> 1
  ti -> 1
  lam -> fun (bodyTrl:int) ->
    bodyTrl
  app -> fun (lamTrl:int) (argTrl:int) ->
    lamTrl + argTrl
  pls -> fun (lTrl:int) (rTrl:int) ->
    lTrl + rTrl + 1
  eq -> fun (lTrl:int) (rTrl:int) ->
    lTrl + rTrl + 1
  ite -> fun (condTrl:int) (ifTrl:int) (elseTrl:int) ->
    condTrl + ifTrl + elseTrl
  alet -> fun (argTrl:int) (bodyTrl:int) ->
    argTrl + bodyTrl
  trpl -> fun (rTrl:int) (tTrl:int) (baTrl:int)
              (bbTrl:int) (tiTrl:int) (lamTrl:int)
              (appTrl:int) (plsTrl:int) (eqTrl:int)
              (iteTrl:int) (aletTrl:int) (trplTrl:int) ->
    rTrl + tTrl + baTrl +
    bbTrl + lamTrl + appTrl +
    plsTrl + eqTrl + iteTrl +
    aletTrl + trplTrl
}

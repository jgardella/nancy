let (x:int) = 1 in
inspect {
  r -> 0
  t -> (fun (x:int) -> fun (y:int) -> x + y)
  ba -> 0
  bb -> 0
  ti -> 0
  lam -> fun (x:int) -> x
  app -> (fun (x:int) -> fun (y:int) -> x + y + 1)
  pls -> (fun (x:int) -> fun (y:int) -> x + y)
  eq -> (fun (x:int) -> fun (y:int) -> x + y)
  ite -> (fun (x:int) -> fun (y:int) -> fun (z:int) -> x + y + z)
  alet -> (fun (x:int) -> fun (y:int) -> x + y)
  trpl -> (
    fun (x1:int) ->
    fun (x2:int) ->
    fun (x3:int) ->
    fun (x4:int) ->
    fun (x5:int) ->
    fun (x6:int) ->
    fun (x7:int) ->
    fun (x8:int) ->
    fun (x9:int) ->
    fun (x10:int) ->
    fun (x11:int) ->
    fun (x12:int) ->
      x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12
  )
}

!inspect {
  r -> false
  t -> (fun x:bool -> fun y:bool -> true)
  ba -> true
  bb -> true
  ti -> false
  lam -> fun x:bool -> true
  app -> (fun x:bool -> fun y:bool -> true)
  let -> (fun x:bool -> fun y:bool -> true)
  trpl -> (
    fun x:bool ->
    fun x:bool ->
    fun x:bool ->
    fun x:bool ->
    fun x:bool ->
    fun x:bool ->
    fun x:bool ->
    fun x:bool ->
    fun x:bool ->
      true
  )
}

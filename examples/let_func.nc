let! u:int->bool =
  (fun x:int -> !(fun a:int -> true)) 1
in
  <u> 3

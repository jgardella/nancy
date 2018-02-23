! let u:int->bool be
  (fun (x:int) -> !(fun (a:int) -> true)) 1
in
  <u> 3

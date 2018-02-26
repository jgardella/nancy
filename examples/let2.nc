let! u:int->bool =
  !fun a:int -> true
in
  let! w:bool->int =
    !fun b:bool -> 1
  in
    <w> (<u> 3)

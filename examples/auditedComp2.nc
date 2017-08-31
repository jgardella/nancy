!x
  let u:int->bool be
    !y fun (a:int) -> true
  in
    let w:bool->int be
      !z fun (b:bool) -> 1
    in
      <z->x | w> (<y->x | u> 3)

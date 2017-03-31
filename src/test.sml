structure Test =
struct
  structure Infer = Infer (Syntax)
  open Infer Syntax

  fun testInfer tm = 
    let
      val ty = infer tm
    in
      print (printTm tm ^ " ==> " ^ printTy ty ^ "\n")
    end

  fun test () = 
    List.app testInfer
      [LAM NIL,
       LAM (VAR 0),
       LAM (LAM (LAM (VAR 2)))]

end
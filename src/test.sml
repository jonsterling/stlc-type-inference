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
    handle TypeError msg => 
      raise Fail ("Failed to infer term " ^ printTm tm ^ " with error: " ^ msg)

  fun test () = 
    List.app testInfer
      [LAM NIL,
       LAM (VAR 0)]

end
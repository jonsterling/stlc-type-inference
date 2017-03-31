structure Syntax :> SYNTAX =
struct
  datatype ty = 
      UNIT
    | ARR of ty * ty

  datatype tm =
      VAR of int
    | LAM of tm
    | AP of tm * tm
    | NIL

  val rec printTy =
    fn UNIT => "unit"
     | ARR (a, b) => "(-> " ^ printTy a ^ " " ^ printTy b ^ ")"

  val rec printTm = 
    fn VAR i => "(var " ^ Int.toString i ^ ")"
     | LAM e => "(lam " ^ printTm e ^ ")"
     | AP (e1, e2) => "(ap " ^ printTm e1 ^ " " ^ printTm e2 ^ ")"
     | NIL => "nil"
end
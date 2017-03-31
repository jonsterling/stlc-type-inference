structure Syntax :> SYNTAX where type ty_var = Meta.sym =
struct
  type ty_var = Meta.sym

  datatype ty = 
      UNIT
    | ARR of ty * ty
    | META of ty_var

  datatype tm =
      VAR of int
    | LAM of tm
    | AP of tm * tm
    | NIL

  val rec eqTy =
    fn (UNIT, UNIT) => true
     | (ARR (a1, b1), ARR (a2, b2)) => eqTy (a1, a2) andalso eqTy (b1, b2)
     | (META i, META j) => Meta.eq (i, j)

  val rec printTy =
    fn UNIT => "unit"
     | ARR (a, b) => "(-> " ^ printTy a ^ " " ^ printTy b ^ ")"
     | META a => Meta.toString a

  val rec printTm = 
    fn VAR i => "(var " ^ Int.toString i ^ ")"
     | LAM e => "(lam " ^ printTm e ^ ")"
     | AP (e1, e2) => "(ap " ^ printTm e1 ^ " " ^ printTm e2 ^ ")"
     | NIL => "nil"
end
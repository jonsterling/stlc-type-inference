signature SYNTAX =
sig
  type ty_var

  datatype ty = 
      UNIT
    | ARR of ty * ty
    | META of ty_var

  datatype tm =
      VAR of int
    | LAM of tm
    | AP of tm * tm
    | NIL

  val eqTy : ty * ty -> bool
  val printTy : ty -> string
  val printTm : tm -> string
end
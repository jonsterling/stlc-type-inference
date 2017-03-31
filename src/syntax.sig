signature SYNTAX =
sig
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

  val eqTy : ty * ty -> bool
  val printTy : ty -> string
  val printTm : tm -> string
end

signature SYNTAX_UTIL =
sig
  include SYNTAX

  datatype equation = =:= of ty * ty
  type constraints = equation list

  type substitution
  val subst : substitution * ty -> ty
  val unify : constraints -> substitution
end
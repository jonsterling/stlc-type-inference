structure Syntax :> SYNTAX =
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

functor SyntaxUtil (Syn : SYNTAX) :> SYNTAX_UTIL where type tm = Syn.tm and type ty = Syn.ty =
struct
  open Syn

  datatype equation = =:= of ty * ty
  infix =:=

  type constraints = equation list

  (* TODO: use a suitable data structure *)
  type substitution = (ty_var * ty) list

  fun lookup (sigma, sym) =
    case sigma of 
       [] => raise Fail "No such metavariable"
     | (sym', ty) :: sigma' => if Meta.eq (sym, sym') then ty else lookup (sigma', sym)

  fun substTy (sigma, ty) = 
    case ty of
       UNIT => UNIT
     | META sym => (lookup (sigma, sym) handle _ => Syn.META sym)
     | ARR (ty1, ty2) => ARR (substTy (sigma, ty1), substTy (sigma, ty2))

  fun substEquation (sigma, ty1 =:= ty2) =
    substTy (sigma, ty1) =:= substTy (sigma, ty2)

  fun substConstraints (sigma, cs) = 
    List.map (fn c => substEquation (sigma, c)) cs

  fun occurs (sym, ty) : bool = 
    case ty of
       UNIT => false
     | META sym' => Meta.eq (sym, sym')
     | ARR (ty1, ty2) => occurs (sym, ty1) orelse occurs (sym, ty2)

  fun occursCheck (sym, ty) =
    if occurs (sym, ty) then 
      raise Fail "Occurs check"
    else
      ()

  fun unify' (cs, sigma) = 
    case cs of
       [] => sigma
     | (META sym =:= ty) :: cs' => unifyMeta (sym, ty, cs', sigma)
     | (ty =:= META sym) :: cs' => unifyMeta (sym, ty, cs', sigma)
     | (ARR (ty11, ty12) =:= ARR (ty21, ty22)) :: cs' => unify' ([ty11 =:= ty21, ty12 =:= ty22] @ cs', sigma)
     | (UNIT =:= UNIT) :: cs' => unify' (cs', sigma)
     | _ => raise Fail "Unification error"

  and unifyMeta (sym, ty, cs, sigma) = 
    let
      val () = occursCheck (sym, ty)
      val sigma' = [(sym, ty)]
      val cs' = substConstraints (sigma', cs)
    in
      unify' (cs', sigma' @ sigma)
    end

  fun unify cs =
    unify' (cs, [])

end
functor Infer(Syn : SYNTAX) :> INFER where type tm = Syn.tm and type ty = Syn.ty =
struct
  type ty = Syn.ty
  type tm = Syn.tm

  datatype meta_ty = 
      UNIT
    | ARR of meta_ty * meta_ty
    | HOLE of Meta.sym

  type ctx = meta_ty list

  datatype equation = =:= of meta_ty * meta_ty
  infix =:=

  type constraints = equation list
  type goal = ctx * tm * meta_ty
  type subgoals = goal list

  type state = subgoals * constraints
  datatype step = STEP of state | RET of constraints

  exception Todo

  (* TODO: use a suitable data structure *)
  type substitution = (Meta.sym * meta_ty) list
  fun lookup (sigma : substitution, sym : Meta.sym) : meta_ty =
    case sigma of 
       [] => raise Fail "No such metavariable"
     | (sym', ty) :: sigma' => if sym = sym' then ty else lookup (sigma', sym)

  fun subst (sigma : substitution, ty : meta_ty) : meta_ty = 
    case ty of
       UNIT => UNIT
     | HOLE sym => (lookup (sigma, sym) handle _ => HOLE sym)
     | ARR (ty1, ty2) => ARR (subst (sigma, ty1), subst (sigma, ty2))

  fun quote (ty : meta_ty) : Syn.ty =
    case ty of 
       UNIT => Syn.UNIT
     | HOLE _ => raise Fail "Hole"
     | ARR (ty1, ty2) => Syn.ARR (quote ty1, quote ty2)

  fun substEquation (sigma : substitution, h : equation) : equation =
    let
      val ty1 =:= ty2 = h
    in
      subst (sigma, ty1) =:= subst (sigma, ty2)
    end

  fun substConstraints (sigma : substitution, cs : constraints) : constraints = 
    List.map (fn c => substEquation (sigma, c)) cs
  
  fun occurs (sym : Meta.sym, ty : meta_ty) : bool = 
    case ty of
       UNIT => false
     | HOLE sym' => sym = sym'
     | ARR (ty1, ty2) => occurs (sym, ty1) orelse occurs (sym, ty2)

  fun occursCheck (sym : Meta.sym, ty : meta_ty) : unit =
    if occurs (sym, ty) then 
      raise Fail "Occurs check"
    else
      ()

  fun unify (cs : constraints, sigma : substitution) : substitution = 
    case cs of
       [] => sigma
     | (HOLE sym =:= ty) :: cs' => unifySym (sym, ty, cs', sigma)
     | (ty =:= HOLE sym) :: cs' => unifySym (sym, ty, cs', sigma)
     | (ARR (ty11, ty12) =:= ARR (ty21, ty22)) :: cs' => unify ([ty11 =:= ty21, ty12 =:= ty22] @ cs', sigma)
     | (UNIT =:= UNIT) :: cs' => unify (cs', sigma)

  and unifySym (sym : Meta.sym, ty : meta_ty, cs : constraints, sigma : substitution) : substitution = 
    let
      val () = occursCheck (sym, ty)
      val sigma' = [(sym, ty)]
      val cs' = substConstraints (sigma', cs)
    in
      unify (cs', sigma' @ sigma)
    end


  local
    fun hole () = 
      HOLE (Meta.fresh ())

    (* The fundamental tactic of type inference, which implements the local adjustments to the 
       inference state incurred by a goal. This includes generation of new subgoals and new 
       equational constraints. *)
    fun rules (goal : goal) : state =
      let
        val (gamma, e, ty) = goal
      in
        case e of
           Syn.VAR i =>
             let
               val ty' = List.nth (gamma, i)
             in
               ([], [ty =:= ty'])
             end
         | Syn.AP (e1, e2) =>
             let
               val ty1 = hole ()
               val goal1 = (gamma, e1, ARR (ty1, ty))
               val goal2 = (gamma, e2, ty1)
             in
               ([goal1, goal2], [])
             end
         | Syn.LAM e' =>
             let
               val ty1 = hole ()
               val ty2 = hole ()
               val goal = (ty1 :: gamma, e', ty2)
             in
               ([goal], [ty =:= ARR (ty1, ty2)])
             end
      end

    (* The type inference state transition: if we have resolved all subgoals, 
       then return the generated constraints. Otherwise, peel off a subgoal and
       proceed. *)
    fun step (s : state) : step =
      case s of
         ([], constraints) => RET constraints
       | (g :: goals, constraints) => 
          let
            val (goals', constraints') = rules g
          in
            STEP (goals' @ goals, constraints' @ constraints)
          end

    (* Repeat the above process until we are finished. *)
    fun eval (s : state) : constraints =
      case step s of
         RET constraints => constraints
       | STEP s' => eval s'

  in
    (* To infer a type, calculate the constraints, exhibit a substitution, and then apply it. *)
    fun infer e = 
      let
        val ty = hole ()
        val goal = ([], e, ty)
        val constraints = eval ([goal], [])
        val sigma = unify (constraints, [])
        val ty' = subst (sigma, ty)
      in
        SOME (quote ty')
      end
      handle _ => NONE
  end
     

  fun check tm ty = 
     case infer tm of
        SOME ty' => ty = ty'
      | NONE => false
end
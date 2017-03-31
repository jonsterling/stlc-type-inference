functor Infer(Syn : SYNTAX) :> INFER where type tm = Syn.tm and type ty = Syn.ty =
struct
  structure Syn = SyntaxUtil (Syn)
  open Syn
  infix =:=

  type ctx = ty list

  type goal = ctx * tm * ty
  type subgoals = goal list

  type state = subgoals * constraints
  datatype step = STEP of state | RET of constraints

  exception TypeError of string

  local
    fun hole () = 
      META (Meta.fresh ())

    (* The fundamental tactic of type inference, which implements the local adjustments to the 
       inference state incurred by a goal. This includes generation of new subgoals and new 
       equational constraints. *)
    fun rules (goal : goal) : state =
      let
        val (gamma, e, ty) = goal
      in
        case e of
           VAR i =>
             let
               val ty' = List.nth (gamma, i)
             in
               ([], [ty =:= ty'])
             end
         | AP (e1, e2) =>
             let
               val ty1 = hole ()
               val goal1 = (gamma, e1, ARR (ty1, ty))
               val goal2 = (gamma, e2, ty1)
             in
               ([goal1, goal2], [])
             end
         | LAM e' =>
             let
               val ty1 = hole ()
               val ty2 = hole ()
               val goal = (ty1 :: gamma, e', ty2)
             in
               ([goal], [ty =:= ARR (ty1, ty2)])
             end
         | NIL => 
             ([], [ty =:= UNIT])
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
    fun check e ty = 
      let
        val goal = ([], e, ty)
        val constraints = eval ([goal], [])
        val sigma = unify constraints
        val ty' = substTy (sigma, ty)
      in
        ty'
      end
      handle TypeError msg => raise TypeError msg
           | exn => raise TypeError ("Weird error: " ^ exnMessage exn)

    fun infer e = 
      check e (hole ())
  end
end
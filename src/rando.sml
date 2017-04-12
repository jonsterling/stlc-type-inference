functor RandomTerms(structure Syn : SYNTAX; structure Rand : RANDOM) :> RANDOM_TERMS
									where type tm = Syn.tm =
struct
open Syn

(* Generate our seed from the seconds. *)
val r = Rand.rand((Date.second o Date.fromTimeLocal o Time.now) ()
		 ,(Date.second o Date.fromTimeLocal o Time.now) () + 13)

(* Convergence constant. This multiplicative factor makes sure we *)
(* eventually converge/terminate. *)

val cp = 0.95

(* Probability of generating each of the following terms. *)
(* We'll generate NIL with probability 1 - probAP - probLAM - probVAR. *)
val probAP = 0.4
val probLAM = 0.4
val probVAR = 0.1

(* Generate a random lambda term using variables in the range [0,n). *)
(* Parameter left is a bool that tells us whether we are on the LHS of *)
(* an application, i.e., whether or not we need to be a LAM to *)
(* be well typed. *)
(* conv is our current convergence factor. *)
fun randtm' n left (conv : real) =
  if left then
      LAM (randtm' (n+1) false (cp * conv))
  else
      let val rr = conv * Rand.randReal r
	  val conv = cp * conv in
	  if rr > 1.0 - probAP then
	      AP (randtm' n true conv, randtm' n false conv)
	  else if rr > 1.0 - probAP - probLAM then
	      LAM (randtm' (n+1) false conv)
	  else if rr > 1.0 - probAP - probLAM - probVAR then
	      if n = 0 then
		  randtm' n false conv
	      else
		  VAR (Rand.randRange (0, n - 1) r)
	  else
	      NIL
      end


fun randtm _ = randtm' 0 false 1.0

end

functor GenExamples(structure Syn : SYNTAX) :>
	sig
	    type tm
	    type ty
	    val genExamples : int -> (tm * ty) list
	    val printTm : tm -> string
	    val printTy : ty -> string
	  end
	  where type tm = Syn.tm and type ty = Syn.ty =
struct

structure Infer = Infer (Syn)
open Syn
open Infer

structure r = RandomTerms(structure Syn = Syn; structure Rand = Random)

fun printTm (VAR i) = Int.toString i
  | printTm (LAM e) = "\\" ^ (printTm e)
  | printTm (AP (e1, e2)) = "(" ^ printTm e1 ^ ")(" ^ printTm e2 ^ ")"
  | printTm NIL = "*"

fun printTy UNIT = "1"
  | printTy (ARR (a, b)) = "(" ^ printTy a ^ ">" ^ printTy b ^ ")"
  | printTy (META _) = "1"

fun simpleTy UNIT = UNIT
  | simpleTy (ARR (a,b)) = ARR (simpleTy a, simpleTy b)
  | simpleTy (META _) = UNIT

fun genExamples n = List.tabulate (n, fn _ => let val tm = r.randtm ()
						  val ty = Infer.infer tm
						  val ety = simpleTy ty
					      in (tm, ety) end)

end

signature INFER =
sig
  type ty
  type tm

  (* Infer or check the type of a closed term. Let me know if you need to support
     open terms, and I can also add this. *)
  val infer : tm -> ty option
  val check : tm -> ty -> bool
end
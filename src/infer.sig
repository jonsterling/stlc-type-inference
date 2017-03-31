signature INFER =
sig
  type ty
  type tm

  exception TypeError of string

  (* Infer the type of a closed term. Let me know if you need to support
     open terms, and I can also add this. *)
  val infer : tm -> ty

  (* Check the type of a closed term, and return a more specific type. *)
  val check : tm -> ty -> ty
end
signature META = 
sig
  type sym
  val fresh : unit -> sym
  val toString : sym -> string
  val eq : sym * sym -> bool
end
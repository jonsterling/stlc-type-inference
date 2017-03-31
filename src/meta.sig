signature META = 
sig
  eqtype sym
  val fresh : unit -> sym
  val toString : sym -> string
  val eq : sym * sym -> bool
end
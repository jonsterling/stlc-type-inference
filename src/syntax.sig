signature SYNTAX =
sig
  datatype ty = 
      UNIT
    | ARR of ty * ty

  datatype tm =
      VAR of int
    | LAM of tm
    | AP of tm * tm
    | NIL of tm
end
structure Meta :> META =
struct
  type sym = int

  val counter = ref 0
  fun fresh () = 
    let
      val sym = !counter
    in
      counter := sym + 1;
      sym
    end
end
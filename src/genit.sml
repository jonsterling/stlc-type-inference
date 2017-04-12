val n = (valOf o Int.fromString o hd o CommandLine.arguments) ()
structure g = GenExamples(structure Syn = Syntax)
val _ = app (fn (x,y) => print (x ^ " " ^ y ^ "\n")) (map (fn (x,y) => (g.printTm x, g.printTy y)) (g.genExamples n))

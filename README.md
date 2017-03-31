### hacking

To run or hack on this, ensure that you have SML/NJ installed. Then, open an SML REPL:

    rlwrap sml

Here's an example session:

    Standard ML of New Jersey...
    - CM.make "src/stlc.cm";
    - Test.test ();

This should print the following:

    (lam nil) ==> (-> #1 unit)
    (lam (var 0)) ==> (-> #4 #4)
    (lam (lam (lam (var 2)))) ==> (-> #7 (-> #9 (-> #11 #7)))

There is no parser yet, but I can write one if we need it.
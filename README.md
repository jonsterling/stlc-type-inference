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


### editors?

I like to edit SML code using [Visual Studio
Code](http://code.visualstudio.com), because we have made a [very nice SML
mode](https://marketplace.visualstudio.com/items?itemName=freebroccolo.sml)
that has mostly correct highlighting (a herculean feat!) and integration with
SML/NJ for type error reporting. (This is what the `sml.json` file is for in
the root directory of this repository.)

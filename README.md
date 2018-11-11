# g0-compiler
Compiler for g0, an instructional subset of Godiva. Made for CS445: Compiler Design class.

Includes:
    Lexer - Using:
        flex v2.5.37
        gcc (GCC) 4.8.5 20150623 (Red Hat 4.8.5-28)


Edge Case Details:

Swap expressions:
    I am not supporting "z = ( x :=: y )" assignments.. It just doesnt make any sense.
    I am only going to allow statements such as "x :=: y;" or without the semicolon.
    
    
Changes that were made since initial submission:
Semicolon insertion was added, still supports optional semi-colons. This means that the semi-colons should work for the majority of stuff, just add the insertion rules wherever necessary.

    
    
Succeeds using the /Examples/HW2_Test/*,

.../HW2_Test/g0stuff.g0 was modified to comment out the one line on 105: "L = [1, 2, 3]" that broke the grammar because it was not supported yet.

In HW #4, many decisions were made:

I am not going to support empty list initializers, e.g.
"
list l;
l = [];
"

This is primarily due to the fact that there would be huge conflicts between empty initializers and default table mappings. anytime a identifier + a "[]" was given,  
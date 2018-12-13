/*
Baylus Tunnicliff
11/16/2018
CS 445 - Compiler Design (g0 - subset of Godiva)
HW #5: Intermediate Code Generation
"codegen.h"
      Begins intermediate code generation.


Key:
"FIX" - these are placed throughout the code where i believe improvement or errors might occur,
   but arent crucial to fix at the moment.
"LOOK" - these are places where i might need to take another look at them later, when i have time,
   as they might just be bad form, improper handling, etc.
*/

char* newlabel(); // Generates labels for code regions

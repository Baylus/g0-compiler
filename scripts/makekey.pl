#!/usr/bin/perl
#Baylus Tunnicliff
#9-6-2018	CS445-Homework 1: Scanner
#"check.pl": Perl script to make checking against my files

use strict;			#Makes it easier to debug.
use warnings;			#Makes it easier to debug.
use 5.010;                      #For using 'given->when' statements.
# use autodie;			#doesnt require appending "|| die() " messages. 

my $d = shift;

my $keyout = "Answers/" . $d . ".key";
system("./g0 $d &> $keyout");


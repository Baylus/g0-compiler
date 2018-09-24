#!/usr/bin/perl
#Baylus Tunnicliff
#9-6-2018	CS445-Homework 1: Scanner
#"check.pl": Perl script to make checking against my files

use strict;			#Makes it easier to debug.
use warnings;			#Makes it easier to debug.
use 5.010;                      #For using 'given->when' statements.
# use autodie;			#doesnt require appending "|| die() " messages. 

my @files = ("art", "bar", "baz", "bbb", "calc", "comments", "div0", "f", "foo", "funcexample", "hello", "if1", "loops");

# system("lex ccx.l");
# system("make");

foreach my $fname (@files) {
	my $myout = "Output/" . $fname . ".txt";
	my $keyout = "Answers/" . $fname . ".key";
	my $checkresults = "results/" . $fname . ".diff_results.txt";
	# system("valgind -v -v -v --leak-check=full --track-origins=yes ./g0 Examples/$fname > $myout");
	system("./g0 Examples/$fname > $myout");
	# system("diff $keyout $myout > $checkresults");
	
	# system("");
	# system("");
	# system("");
	
}

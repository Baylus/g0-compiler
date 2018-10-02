#!/usr/bin/perl
#Baylus Tunnicliff
#9-6-2018	CS445-Homework 1: Scanner
#"check.pl": Perl script to make checking against my files

use strict;			#Makes it easier to debug.
use warnings;			#Makes it easier to debug.
use 5.010;                      #For using 'given->when' statements.
# use autodie;			#doesnt require appending "|| die() " messages. 

my $d = shift;

opendir(D, "$d") || die "Can't open directory $d: $!\n";
my @list = grep { (!/^\./) && -f "$d/$_" } readdir(D);
closedir(D);

my @files = ("art", "bar", "baz", "bbb", "calc", "comments", "div0", "f", "foo", "funcexample", "hello", "if1", "loops");

foreach my $fname (@list) {
	my $myout = "Output/" . $d . "/" . $fname . ".txt";
	my $keyout = "Answers/" . $d . "/" . $fname . ".key";
	my $checkresults = "results/" . $d . "/" . $fname . ".diff_results.txt";
	# system("valgind -v -v -v --leak-check=full --track-origins=yes ./g0 Examples/$fname > $myout");
	# system("./g0 $d/$fname &> $myout");
	system("./g0 $d/$fname");
	# system("diff $keyout $myout > $checkresults");
}

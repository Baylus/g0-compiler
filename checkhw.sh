#!/bin/sh

#MY_OUT=$1.txt
#WILD_OUT=$1.lexer.out
#cat $1
#./lexer.exe $1
#diff --normal $MY_OUT $WILD_OUT

#if -f "$1"; then
if [[ -f "$1" ]]; then
        MYOUT="$1.txt"
        LEXOUT="$1.lexer.out"
        # ./lexer.exe "$1" > debug.txt
	gcc -g lexer.c
        ./a.out "$1"
        # diff --normal $MYOUT $LEXOUT > /dev/null
        diff --normal $MYOUT $LEXOUT
		# diff --brief <(sort $MYOUT) <(sort $LEXOUT) >/dev/null
		# comp_value=$?
		# if [ $comp_value -eq 1 ]
		# then
			# echo "	Files are different"
		# else
			# echo "	Files are the same!!!"
		# fi
else
        cat "$1"
        cat "File Not Found"
fi



#!/bin/bash
if [ $# -ne 1 ]; then
	echo usage : ./aps.sh [filename]
else
echo "checking type for" $1
t=$(./toProlog $1 | swipl -s typer.pl -g checker 2>/dev/null)
if [ $t = "true" ]; then
	echo "typage ok"
	echo $(./eval $1)
else
	echo "probleme de typage"
fi
fi

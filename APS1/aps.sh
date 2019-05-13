#!/bin/bash
for x in exemple/*.aps;
do
echo "checking type for" $x
t=$(./toProlog $x | swipl -s typer.pl -g checker 2>/dev/null)
echo $(./toProlog $x)
if [ $t = "true" ]; then
	echo "typage ok"
	echo $(./eval $x)
else
	echo "probleme de typage"
fi
 echo -e "\n"

done
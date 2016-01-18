# replace.sh
# Replace pattern 1 with pattern 2 in <file>
# usage: ./replace.sh <file>

cp $1 $1.tmp
scala scalation.util.Replace < $1.tmp > $1


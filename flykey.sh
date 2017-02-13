#!/bin/bash
# flkey.sh --- A script to produce a list of Emacs keybinding commands.

# The contents of the .flyk buffer are assumed to be in $1.

# Delete the last line and double any backslashes for correct evaluation
# of the string in Emacs lisp.
echo "$1" | sed '$d' | awk -F'\' '
NR>1 {out=$1; for(i=2;i<=NF;i++){out=out"\\\\"$i}; print out}
' |\
    # Create the keybinding commands.
    awk -F= '
{print "(local-set-key (kbd \"" $1 "\") (lambda () (interactive) (insert \"" $2 "\")))"}'

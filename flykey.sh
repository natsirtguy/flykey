#!/bin/bash
# flkey.sh --- A script to produce a list of Emacs keybinding commands.

# The FlyKey file specifying the keybindings is assumed to be in $1.

# Double any backslashes for correct evaluation of the string in Emacs lisp.
awk -F'\' '
NR>1 {out=$1; for(i=2;i<=NF;i++){out=out"\\\\"$i}; print out}
' $1 |
    # Write the keybinding commands
    awk -F= '
{print 
"(define-key newmap (kbd \"" $1 "\") (lambda () (interactive) (insert \"" $2 "\")))"}'

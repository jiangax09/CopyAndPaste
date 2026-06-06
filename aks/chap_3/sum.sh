#!/usr/bin/awk -f

NR == 2 {
    nfld = NF
    for (i = 1; i <= NF; i++)
        numcol[i] = isnum($i)
}
NR >= 2 && NF > 0 {
    for (i = 1; i <= nfld; i++)
        if (numcol[i])
            sum[i] += $i
}

END {
    for (i = 1; i <= nfld; i++) {
          if (numcol[i])
             printf("%g", sum[i])
          else
             printf("--")

          printf(i < NF ? "\t" : "\n")
       }
    }

function isnum(n) {return n + 0 == n}
#{ return n ~ /^[+-]?[0-9]+$/ }

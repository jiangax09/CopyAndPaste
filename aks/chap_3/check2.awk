#!/usr/bin/awk -f
## awk -v key1=date -v key2=amount -f check2.awk input.txt

BEGIN {RS = ""; FS = "\n" }
/(^|\n)check/ {
    delete val

    for (i = 1; i <= NF; i++) {
        split($i, fs, "\t")
        val[f[1]] = f[2]
    }

    printf("%8s %5d", val[key1], val[key2])
}

#!/usr/bin/awk -f

BEGIN {FS = "!!#"}
{
    # printf("%s!!# ", x[split($1, x, " ")])
    for (i = 2; i <= NF; i++) {
        printf("%s\n", $i)
    }
    printf("\n")
}

#!/usr/bin/awk -f
BEGIN { FS = "[\t ]+" }
NR >= 2 {
    #printf("%s:%s:%d:%d\n", $4, $1, $3, $2)
    #printf("%.1f\n", $3 / $2)
    printf("%s:%s:%d:%d:%.1f\n", $1, $2, $3, $4, 1000 * $3 / $4) | "sort -t: -k1,1 -k2,2r -k5,5rn"
}

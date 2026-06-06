#!/usr/bin/awk -f

function daynum(y, m, d,    days, i, n) {
    split("31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31", data)
    n = (y - 1901) * 365 + int((y - 1901) / 4)
    if (y % 4 == 0)
       days[2]++
    for (i = 1; i <= m; i++)
        n += days[i]
    return n + d
}

{
    print daynum($1, $2, $3)
}

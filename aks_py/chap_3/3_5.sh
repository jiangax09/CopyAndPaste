#!/usr/bin/awk -f
BEGIN {
    BUCKS = 20
}

NR == 1 {
    min = max = int($1)
}

{
    num = int($1)
    if (num < min)
        min = num
    if (num > max)
        max = num

    data[NR] = num
}

END {
    bucket_width = int((max - min + 1 + BUCKS - 1) / BUCKS)
    if (bucket_width < 1)
        bucket_width = 1

    for (r = 1; i <= NR; i++) {
        buck_idx = int((data[i] - min) / bucket_width)
        if (buck_idx >= BUCKS)
            buck_idx = BUCKS - 1

        x[buck_idx]++
    }
    for (i = 0; i < BUCKS; i++) {
        low = min + i * bucket_width
        high = low + buck_width - 1
        printf(" %2d - %2d: %3d %s\n",
              low, high, x[i], rep(x[i], "*"))
    }
    printf("%3d:   %3d %s\n", max, x[BUCKS], rep(x[BUCKS], "*"))
}

function rep(n, s, t) {
    while (n-- > 0)
          t = t s
    return t
}

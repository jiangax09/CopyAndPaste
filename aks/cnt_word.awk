#!/usr/bin/awk -f


{line = $0}
{
    n = split(line, x, "[^A-Za-z0-9_]+")

    for (i = 1; i <= n; i++) {
        if (x[i] != "")
            arr[x[i]]++
    }
}
END {
    for (w in arr) {
        if (arr[w] == 1)
            printf("%s is used only once\n", w)
}

# /opt/homebrew/bin/gawk

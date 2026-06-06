#!/usr/bin/awk -f

BEGIN {
    asplit("close system atan2 sin match sub gsub", fcns)
    asplit("ARGC ARGV FNR RESTART RLENGTH SUBSEP", vars)
    asplit("do delete function return", keys)
}

{line = $0}
/"/  {gsub(/"([^"]|\\)*"/, "", line)}
/\// {gsub(/\/([^\/]|\\\/)+\//, "", line)}
/#/  {sub(/#.*/, "", line)}
/\<(do|delete|functio|return)\>/ {
    warn(line)
}
{
    n = split(line, x, "[^A-Za-z0-9_]+")
    for (i = 1; i <= n; i++) {
        if (x[i] in fcns)
            warn(x[i] " is now a built-in function")
        if (x[i] in vars)
            warn(x[i] " is now a built-in var")
        if (x[i] in keys)
            warn(x[i] " is now a built-in key")
    }
}

function asplit(str, arr) {
    n = split(str, temp)
    for (i = 1; i <= n; i++)
        arr[temp[i]]++
    return n
}

function warn(s) {
    sub(/^[ \t]*/, "")
    printf("file %s, line %d: %s\n\t%s\n", FILENAME, FNR, s, $0)
}

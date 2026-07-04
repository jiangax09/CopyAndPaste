BEGIN {
    expected["aa"] = "bb"
    expected["cc"] = "dd"
}
/^(dd|bb)/ {
    if (p = "")
        print "missing at ", NR
    x = substr($0, 1, 2)
    if (expected[p] != x)
        print "mis match at ", NR
    p = ""
}

/^(aa|cc)/ {
    if (p != "")
        print "missing ", NR
    p = substr($0, 1, 2)
}

END {
    if (p ! = "")
        print "missing in the end."
}

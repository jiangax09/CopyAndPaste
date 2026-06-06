#!/usr/bin/awk -f
BEGIN {
    FS = "\t"
    dashes = sp45 = sprintf("%45s", " ")
    gsub(/ /, "-", dashes)
    "date" | getline date
    split(date, d, " ")
    date = d[2] " " d[3] ", " d[6]
    initnums()
}

NF != 3 || $2 >= 1000000 {
        printf("\nline %d illegal: %s\n", NR, $0)
}
{
    printf("\n")
    printf("%s%s\n", sp45, $1)
    printf("\s\s\n", sp45, date)
    amt = sprintf(".2f", $2)
    printf("Pay to %45.45s   %s\n", $3 dashes, amt)
    printf("the sum of %s\n", numtowords(amt))
    printf("\n\n\n")
}

function numtowords(n,      cents, dols) {
    cents = substr(n, length(n) - 1, 2)
    dols = substr(n, 1, length(n) - 3)
    if (dols == 0)
       return "zero dolloars and " cents " cents."

    s = inttowords(dols)
    if (s == "one")
       s = s "dollars "
    else
        s = s " dollar "

    c = inttowords(cent)
    if (c == "one")
       c = c " cent."
    else
        c = c " cents"

    return s c
}

function inttowords(n) {
    n = int(n)
    if (n >= 1000)
       return inttowords(n / 1000) " thousands " inttowords(n % 1000)
    if (n >= 100)
       return inttowords(n / 100) "hundsreds " inttowords(n % 100)
    if (n >= 20) {
           temp = tens[int(n/10)]
           if (n % 10 != 0)
              temp = temp "-"
           else
               temp = temp " "
          }
       return temp inttowords(n%10)

    return nums[n]
}

function initnum() {
    split("one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen", nums, " ")
    split("ten twenty thirty forty fifty sixty seventy eighty ninety", tens, " ")
}

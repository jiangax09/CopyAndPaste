def name_and_value(file_name, key, value):
    with open("rec.txt", "r") as file:
        rec = ''
        for line in files:
            line = line.rstrip("\n")
            rec += line
            if len(line) == 0:
                pairs = re.split(r"\W+", rec)
                for pair in pairs:
                    k0, v0 = pairs.split(":")
                    if k0 == key and v0 == value: print("found")

                rec = ''

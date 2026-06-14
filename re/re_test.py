from collections import defaultdict
import re
by=defaultdict(lambda: {'stores':[], 'loads':[]})
print(f"by = {by}")
p = re.compile(r"\\section")
print(f"test = {p.match('abc')}")

m = p.match('\section{\section{}}')
if m:
    print(f"group, start = {m.group(), m.start()}")
else:
    print('not found')

p1 = re.compile(r"(C{1,3}D?|DC{0,3}|CM)")

for s in ['C', 'CCC', 'CC', 'CD', 'D', 'DC', 'DCC', 'DCCC', 'CM']:
    print(f"match = {p1.match(s)}")

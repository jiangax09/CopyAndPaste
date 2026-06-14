import re
from collections import defaultdict, Counter
path='//Users/zengwu/cpps/CopyAndPaste/re/attn_fwd_ws_kernel.amdgcn'

# scratch_store_b64 off, v[64:65], s33 offset:256 nv
# scratch_load_b64 v[2:3], off, s33 offset:136 th:TH_LOAD_LU nv ; 8-byte Folded Reload
# scratch_store_b32 off, v254, s33 offset:728 nv ; 4-byte Folded Spill
# scratch_load_b32 v0, off, s33 offset:88 nv ; 4-byte Folded Reload
lines=open(path).read().splitlines()
spills=[]
reloads=[]
store_pattern = re.compile(r'scratch_store_b32\s+off,\s+v(\d+),\s+\w+\s+offset:(\d+)\s+')
load_pattern = re.compile(r'scratch_load_b32\s+v(\d+),\s+off,\s+s33\s+offset:(\d+)\s+')
for i,l in enumerate(lines,1):
    m=store_pattern.search(l)
    if m:
        off=int(m.group(2) or 0); spills.append((off,i,l.strip()))

    m=load_pattern.search(l)
    if m:
        off=int(m.group(2) or 0); reloads.append((off,i,l.strip()))

print('\nspills count',len(spills),'reloads count',len(reloads),'unique spill offsets',len(set(o for o,_,__ in spills+reloads)))

by=defaultdict(lambda: {'stores':[], 'loads':[]})
for off,i,l in spills:
    by[off]['stores'].append((i,l))
for off,i,l in reloads:
    by[off]['loads'].append((i,l))

print('spill slots ranked by lifetime span:')
rows=[]
for off,d in by.items():
    all_lines=[i for i,_ in d['stores']+d['loads']]

    rows.append((max(all_lines)-min(all_lines), off, len(d['stores']), len(d['loads']), min(all_lines), max(all_lines)))

for span,off,ns,nl,first,last in sorted(rows, reverse=True)[:40]:
    print(f'off {off:3d}: span {span:5d} lines {first}-{last}, stores {ns}, loads {nl}')
    #print('  first store:', by[off]['stores'][0] if by[off]['stores'] else None)
    #print('  first load :', by[off]['loads'][0] if by[off]['loads'] else None)
exit()

cnt=Counter()
for l in lines:
    s=l.strip()
    if not s or s.startswith(';') or s.startswith('.') or s.startswith('#') or s.startswith('-'): continue
    op=s.split()[0]
    cnt[op]+=1

print('\ntop opcodes:')
for op,n in cnt.most_common(30): print(op,n)

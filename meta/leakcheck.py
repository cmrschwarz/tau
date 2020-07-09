#!/usr/bin/env python3
import sys
import re
import os

f = sys.argv[1]

if not os.path.isfile(f):
    print("please pass a file")
    exit(-1)

with open(sys.argv[1]) as f: 
    lines = f.read().splitlines()
allocs = {}
alloc = re.compile("^alloc.*?([0-9xA-Fa-f]+).*?$")
free = re.compile("^free.*?([0-9xA-Fa-f]+).*?$")
for idx, line in enumerate(lines):
    a = alloc.match(line)
    if a:
        allocs[a.groups(1)] = idx
    else:
        f = free.match(line)
        if f:
            addr = f.groups(1)
            if addr in allocs:
                lines[allocs[addr]] = None
                lines[idx] = None
                allocs.pop(addr)
            else:
                lines[idx] = lines[idx] + "(!!unmatched free)"
for idx in allocs.values():
    lines[idx] = lines[idx] + "(!!unmatched alloc)"

for l in lines:
    if l is not None:
        print(l)

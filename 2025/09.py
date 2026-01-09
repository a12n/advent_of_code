#!/usr/bin/env python3

import matplotlib.collections as mc
import matplotlib.patches as mp
import matplotlib.pyplot as plt
import sys

xs = []
ys = []
# lines = []
for line in sys.stdin:
    x, y = tuple(map(int, map(str.strip, line.split(','))))
    xs.append(x)
    ys.append(y)
    # a, b = line.split()
    # ax, ay = tuple(map(int, a.split(',')))
    # bx, by = tuple(map(int, b.split(',')))
    # lines.append(((ax, ay), (bx, by)))

fig, ax = plt.subplots()
ax.set_xlim(0, 100000)
ax.set_ylim(0, 100000)

ax.plot(xs, ys, '-o', ms=2)
# ax.add_collection(mc.LineCollection(lines))
ax.axvline(x=94651, alpha=0.5, color='r')
ax.axhline(y=33528, alpha=0.5, color='r')
ax.axhline(y=68153, alpha=0.5, color='r')

plt.grid()
plt.show()

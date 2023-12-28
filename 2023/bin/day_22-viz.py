#!/usr/bin/env python3

import functools
import operator
import sys

import matplotlib.colors as mcolors
import matplotlib.pyplot as plt
import numpy

dimensions = tuple(map(int, sys.stdin.readline().split()))
x, y, z = numpy.indices(dimensions)

bricks = []
for line in sys.stdin:
    x1, y1, z1, x2, y2, z2 = tuple(map(int, line.split()))
    bricks.append((x >= x1) & (x <= x2) & (y >= y1) &
                  (y <= y2) & (z >= z1) & (z <= z2))

# Combine bricks into a single boolean array.
voxels = functools.reduce(operator.or_, bricks)

# Set color of each object.
all_colors = list(mcolors.TABLEAU_COLORS.keys())
colors = numpy.empty(voxels.shape, dtype=object)
for i in range(len(bricks)):
    colors[bricks[i]] = all_colors[i % len(all_colors)]

# Plot everything.
ax = plt.figure().add_subplot(projection='3d')
ax.voxels(voxels, facecolors=colors, edgecolor='k')
plt.show()

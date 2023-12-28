#!/usr/bin/env python3

import functools
import operator
import sys

import matplotlib.colors as mcolors
import matplotlib.pyplot as plt
import numpy

brick_spans = []
dimensions = (0, 0, 0)
for line in sys.stdin:
    if line.strip().startswith('#'):
        continue
    pmin, pmax = tuple(line.split('~'))
    pmin = tuple(map(int, pmin.split(',')))
    pmax = tuple(map(int, pmax.split(',')))
    brick_spans.append((pmin, pmax))
    dimensions = numpy.maximum(dimensions, pmax)

x, y, z = numpy.indices(dimensions + 1)
bricks = [(x >= x1) & (x <= x2) & (y >= y1) &
          (y <= y2) & (z >= z1) & (z <= z2) \
          for ((x1, y1, z1), (x2, y2, z2)) in brick_spans]

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

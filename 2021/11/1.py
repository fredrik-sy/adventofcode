from io import open
import numpy as np

with open('input.txt') as io:
    level = np.array([list(map(int, list(line)))
                     for line in io.read().splitlines()])
    total = 0
    for _ in range(100):
        level += 1
        explored = set()
        updated = True
        while updated:
            updated = False
            for y, x in np.argwhere(level > 9):
                if (y, x) not in explored:
                    explored.add((y, x))
                    updated = True
                    miny = y - 1
                    maxy = y + 1
                    minx = x - 1
                    maxx = x + 1
                    if miny >= 0:
                        if minx >= 0:
                            level[miny, minx] += 1
                        if maxx < np.size(level, 1):
                            level[miny, maxx] += 1
                        level[miny, x] += 1
                    if maxy < np.size(level, 0):
                        if minx >= 0:
                            level[maxy, minx] += 1
                        if maxx < np.size(level, 1):
                            level[maxy, maxx] += 1
                        level[maxy, x] += 1
                    if minx >= 0:
                        level[y, minx] += 1
                    if maxx < np.size(level, 1):
                        level[y, maxx] += 1
        total += np.count_nonzero(level > 9)
        level[level > 9] = 0
    print(total)

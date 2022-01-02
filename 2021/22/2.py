import re
import numpy as np


class Cuboid:
    def __init__(self, on, minx, maxx, miny, maxy, minz, maxz) -> None:
        self.on = on
        self.minx = minx
        self.maxx = maxx + 1
        self.miny = miny
        self.maxy = maxy + 1
        self.minz = minz
        self.maxz = maxz + 1

    def __repr__(self) -> str:
        return f'{self.on} x={self.minx}..{self.maxx},y={self.miny}..{self.maxy},z={self.minz}..{self.maxz}'


with open('input.txt') as io:
    cuboids = [Cuboid(
        int(match.group(1) == 'on'),
        int(match.group(2)),
        int(match.group(3)),
        int(match.group(4)),
        int(match.group(5)),
        int(match.group(6)),
        int(match.group(7))) for match in re.finditer(
        r'(on|off) x=([0-9-]+)..([0-9-]+),y=([0-9-]+)..([0-9-]+),z=([0-9-]+)..([0-9-]+)', io.read())]
    x = []
    y = []
    z = []
    for cuboid in cuboids:
        x.append(cuboid.minx)
        x.append(cuboid.maxx)
        y.append(cuboid.miny)
        y.append(cuboid.maxy)
        z.append(cuboid.minz)
        z.append(cuboid.maxz)
    x.sort()
    y.sort()
    z.sort()
    grid = np.zeros((len(x), len(y), len(z)), int)
    for cuboid in cuboids:
        x0 = x.index(cuboid.minx)
        x1 = x.index(cuboid.maxx)
        y0 = y.index(cuboid.miny)
        y1 = y.index(cuboid.maxy)
        z0 = z.index(cuboid.minz)
        z1 = z.index(cuboid.maxz)
        for xn in range(x0, x1):
            for yn in range(y0, y1):
                for zn in range(z0, z1):
                    grid[xn][yn][zn] = cuboid.on
    total = 0
    for xn in range(len(x) - 1):
        for yn in range(len(y) - 1):
            for zn in range(len(z) - 1):
                xd = x[xn + 1] - x[xn]
                yd = y[yn + 1] - y[yn]
                zd = z[zn + 1] - z[zn]
                total += grid[xn][yn][zn] * xd * yd * zd
    print(total)

import re
import numpy as np
from itertools import product


class Cuboid:
    def __init__(self, match) -> None:
        self.status = 1 if match.group(1) == 'on' else 0
        self.minx = int(match.group(2))
        self.maxx = int(match.group(3))
        self.miny = int(match.group(4))
        self.maxy = int(match.group(5))
        self.minz = int(match.group(6))
        self.maxz = int(match.group(7))

    def __repr__(self) -> str:
        return f'{self.status} x={self.minx}..{self.maxx},y={self.miny}..{self.maxy},z={self.minz}..{self.maxz}'

    def __iter__(self):
        return product(
            range(self.minx, self.maxx + 1),
            range(self.miny, self.maxy + 1),
            range(self.minz, self.maxz + 1))

    def in_range(self, min, max):
        return min <= self.minx and self.maxx <= max and min <= self.miny and self.maxy <= max and min <= self.minz and self.maxz <= max


with open('input.txt') as io:
    cuboids = [Cuboid(match) for match in re.finditer(
        r'(on|off) x=([0-9-]+)..([0-9-]+),y=([0-9-]+)..([0-9-]+),z=([0-9-]+)..([0-9-]+)', io.read())]
    grid = {}
    for cuboid in cuboids:
        if cuboid.in_range(-50, 50):
            for cube in cuboid:
                grid[cube] = cuboid.status
    print(sum(grid.values()))

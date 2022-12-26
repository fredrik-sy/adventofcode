
from dataclasses import dataclass
from io import open
from itertools import cycle
import numpy as np


def input():
    with open('input.txt') as io:
        return set(map(lambda line: tuple(map(int, line.split(','))), io.read().splitlines()))


def part1():
    cubes = input()
    surface_area = 0

    for x, y, z in cubes:
        exposed = 6

        for i in (-1, 1):
            if (x + i, y, z) in cubes:
                exposed -= 1

            if (x, y + i, z) in cubes:
                exposed -= 1

            if (x, y, z + 1) in cubes:
                exposed -= 1

        surface_area += exposed

    print(f'Part 1: {surface_area}')


def in_range(value):
    return -1 <= value <= 20


def neighbors(x, y, z):
    for i in (-1, 1):
        if in_range(x + i):
            yield x + i, y, z

        if in_range(y + i):
            yield x, y + i, z

        if in_range(z + i):
            yield x, y, z + i


def part2():
    cubes = input()
    queue = {(0, 0, 0)}
    visited = set()
    surface_area = 0

    while queue:
        x, y, z = queue.pop()
        visited.add((x, y, z))

        for nx, ny, nz in neighbors(x, y, z):
            if (nx, ny, nz) not in visited:
                if (nx, ny, nz) in cubes:
                    surface_area += 1
                else:
                    queue.add((nx, ny, nz))

    print(f'Part 2: {surface_area}')


if __name__ == '__main__':
    part1()
    part2()

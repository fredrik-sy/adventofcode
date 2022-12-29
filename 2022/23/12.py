from __future__ import annotations
from collections import defaultdict
from dataclasses import dataclass
from enum import IntEnum
from io import open
from re import findall
from sys import maxsize
import numpy as np


class Direction(IntEnum):
    NORTH = 0
    SOUTH = 1
    WEST = 2
    EAST = 3


def input():
    coord = set()

    with open('input.txt') as io:
        for y, line in enumerate(io.read().splitlines()):
            for x, char in enumerate(line):
                if char == '#':
                    coord.add((x, y))

    return coord


def north_coord(x, y):
    return [(ox, y - 1) for ox in range(x - 1, x + 2)]


def south_coord(x, y):
    return [(ox, y + 1) for ox in range(x - 1, x + 2)]


def west_coord(x, y):
    return [(x - 1, oy) for oy in range(y - 1, y + 2)]


def east_coord(x, y):
    return [(x + 1, oy) for oy in range(y - 1, y + 2)]


def adjacent_coord(x, y):
    return [(ox, oy) for oy in range(y - 1, y + 2) for ox in range(x - 1, x + 2) if (ox, oy) != (x, y)]


def area(coord):
    minx = miny = maxsize
    maxx = maxy = -maxsize - 1

    for x, y in coord:
        if x < minx:
            minx = x

        if x > maxx:
            maxx = x

        if y < miny:
            miny = y

        if y > maxy:
            maxy = y

    return (maxx - minx + 1) * (maxy - miny + 1)


def process(coord: set, direction):
    next_coord = defaultdict(list)

    for x, y in coord:
        if not any(xy in coord for xy in adjacent_coord(x, y)):
            continue

        for i in range(4):
            dir = (direction + i) % 4

            match dir:
                case Direction.NORTH:
                    if not any(xy in coord for xy in north_coord(x, y)):
                        xy = (x, y - 1)
                        next_coord[xy].append((x, y))
                        break
                case Direction.SOUTH:
                    if not any(xy in coord for xy in south_coord(x, y)):
                        xy = (x, y + 1)
                        next_coord[xy].append((x, y))
                        break
                case Direction.WEST:
                    if not any(xy in coord for xy in west_coord(x, y)):
                        xy = (x - 1, y)
                        next_coord[xy].append((x, y))
                        break
                case Direction.EAST:
                    if not any(xy in coord for xy in east_coord(x, y)):
                        xy = (x + 1, y)
                        next_coord[xy].append((x, y))
                        break

    for xy, prev_coord in next_coord.items():
        if len(prev_coord) == 1:
            coord.difference_update(prev_coord)
            coord.add(xy)

    return next_coord


def part1():
    coord = input()
    direction = Direction.NORTH

    for _ in range(10):
        process(coord, direction)
        direction = (direction + 1) % 4

    print(f'Part 1: {area(coord) - len(coord)}')


def part2():
    coord = input()
    direction = Direction.NORTH
    rounds = 0

    while True:
        rounds += 1
        next_coord = process(coord, direction)
        direction = (direction + 1) % 4

        if not next_coord:
            break

    print(f'Part 2: {rounds}')


if __name__ == '__main__':
    part1()
    part2()

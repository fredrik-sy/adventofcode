from typing import Deque, Dict, Set, Tuple
import numpy as np
from scipy.spatial.transform import Rotation as R
from itertools import product
from operator import add, sub
from pandas import DataFrame


def parse_scanner(scanner: str):
    return np.array([list(map(int, coordinate.split(','))) for coordinate in scanner.splitlines()[1:]])


def most_common(l, r):
    common = {}  # type: Dict[Tuple, Set]
    for lx, ly, lz in l:
        for rx, ry, rz in r:
            position = lx - rx, ly - ry, lz - rz
            common.setdefault(position, set()).add((rx, ry, rz))
    return max(common.items(), key=lambda item: len(item[1]))


def search(scanner_1, scanner_x):
    for scanner_x in rotations(scanner_x):
        position, common = most_common(scanner_1, scanner_x)
        if len(common) >= 12:
            scanner_positions.append(position)
            return np.array([tuple(map(add, beacon, position)) for beacon in scanner_x])
    raise Exception("NotFound")


def rotations(scanner):
    scanner_r = scanner
    for _ in range(4):
        scanner_r = rotate(scanner_r, 90, 'x')
        for _ in range(4):
            scanner_r = rotate(scanner_r, 90, 'z')
            yield scanner_r
    scanner_r = rotate(scanner, -90, 'y')
    for _ in range(4):
        scanner_r = rotate(scanner_r, 90, 'z')
        yield scanner_r
    scanner_r = rotate(scanner, 90, 'y')
    for _ in range(4):
        scanner_r = rotate(scanner_r, 90, 'z')
        yield scanner_r


def rotate(scanner, degree, axis):
    if axis == 'x':
        rotation_axis = np.array([1, 0, 0])
    elif axis == 'y':
        rotation_axis = np.array([0, 1, 0])
    else:
        rotation_axis = np.array([0, 0, 1])
    rotation = R.from_rotvec(np.radians(degree) * rotation_axis)
    return np.rint(rotation.apply(scanner)).astype(int)


def manhattan_distance(l, r):
    return abs(l[0] - r[0]) + abs(l[1] - r[1]) + abs(l[2] - r[2])


with open('input.txt') as io:
    numbers = Deque(parse_scanner(scanner)
                    for scanner in io.read().split('\n\n'))
    scanner_1 = numbers.popleft()
    scanner_positions = [(0, 0, 0)]
    loop = len(numbers)
    while loop:
        scanner_x = numbers.popleft()
        try:
            scanner_1 = np.concatenate((scanner_1,
                                        search(scanner_1, scanner_x)))
            scanner_1 = DataFrame(scanner_1).drop_duplicates().values
            loop = len(numbers)
        except:
            numbers.append(scanner_x)
            loop -= 1
    if not numbers:
        print(max(manhattan_distance(scanner_positions[i], scanner_positions[j])
                  for i in range(len(scanner_positions) - 1)
                  for j in range(i + 1, len(scanner_positions))))

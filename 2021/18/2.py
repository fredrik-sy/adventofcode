import json
import math
from typing import Union
from itertools import permutations


class Point:
    def __init__(self, x, y, depth, parent=None) -> None:
        self.x: Union[Point, int] = x
        self.y: Union[Point, int] = y
        self.depth = depth
        self.parent: Union[Point, None] = parent

        if isinstance(self.x, Point):
            self.x.parent = self
        if isinstance(self.y, Point):
            self.y.parent = self

    def __repr__(self) -> str:
        return f'[{self.x}, {self.y}]'

    def all(self):
        array = set()
        array.add(self)
        if isinstance(self.x, Point):
            array.update(self.x.all())
        if isinstance(self.y, Point):
            array.update(self.y.all())
        return array

    def explode(self):
        flatten = self.root().flatten()
        for i in range(len(flatten)):
            if flatten[i] is self:
                if i - 1 >= 0:
                    if (isinstance(flatten[i - 1].x, int) and isinstance(flatten[i - 1].y, int)) or isinstance(flatten[i - 1].y, int):
                        flatten[i - 1].y += self.x
                    else:
                        flatten[i - 1].x += self.x
                if i + 1 < len(flatten):
                    if (isinstance(flatten[i + 1].x, int) and isinstance(flatten[i + 1].y, int)) or isinstance(flatten[i + 1].x, int):
                        flatten[i + 1].x += self.y
                    else:
                        flatten[i + 1].y += self.y
                if self.parent.x is self:
                    self.parent.x = 0
                elif self.parent.y is self:
                    self.parent.y = 0
                break

    def filter(self, func):
        return [point for point in self.flatten() if func(point)]

    def flatten(self):
        flatten = []
        if isinstance(self.x, Point):
            flatten.extend(self.x.flatten())
        if isinstance(self.x, int) or isinstance(self.y, int):
            flatten.append(self)
        if isinstance(self.y, Point):
            flatten.extend(self.y.flatten())
        return flatten

    def magnitude(self):
        x = self.x
        y = self.y
        if isinstance(x, Point):
            x = x.magnitude()
        if isinstance(y, Point):
            y = y.magnitude()
        return 3 * x + 2 * y

    def root(self):
        if self.parent:
            return self.parent.root()
        else:
            return self

    def split(self):
        if isinstance(self.x, int) and self.x >= 10:
            self.x = Point(math.floor(self.x / 2),
                           math.ceil(self.x / 2),
                           self.depth + 1,
                           self)
        elif isinstance(self.y, int) and self.y >= 10:
            self.y = Point(math.floor(self.y / 2),
                           math.ceil(self.y / 2),
                           self.depth + 1,
                           self)


def connect(x, y, depth):
    if isinstance(x, list):
        x = connect(x[0], x[1], depth + 1)
    if isinstance(y, list):
        y = connect(y[0], y[1], depth + 1)
    return Point(x, y, depth)


def reduce(point: Point):
    while point.filter(lambda p: p.depth > 4 or (isinstance(p.x, int) and p.x >= 10) or (isinstance(p.y, int) and p.y >= 10)):
        while point.filter(lambda p: p.depth > 4):
            for p in point.filter(lambda p: p.depth > 4 and isinstance(p.x, int) and isinstance(p.y, int)):
                p.explode()
        for p in point.filter(lambda p: (isinstance(p.x, int) and p.x >= 10) or (isinstance(p.y, int) and p.y >= 10)):
            p.split()
            break


with open('input.txt') as io:
    numbers = [json.loads(line) for line in io.readlines()]
    largest = []
    for x, y in list(permutations(numbers, 2)):
        connected = connect(x, y, depth=1)
        reduce(connected)
        largest.append(connected.magnitude())
    print(max(largest))

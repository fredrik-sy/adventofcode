from io import open
from collections import namedtuple
import numpy as np

Point = namedtuple('Point', ['x', 'y'])


def create_point(point):
    return Point(*map(int, point.split(',')))


def generate_range(l: Point, r: Point):
    if l.x == r.x:
        for y in range(abs(l.y - r.y) + 1):
            yield l.x, min(l.y, r.y) + y
    elif l.y == r.y:
        for x in range(abs(l.x - r.x) + 1):
            yield min(l.x, r.x) + x, l.y
    else:
        return []


def generate_coordinates(input):
    for lines in input:
        for point in generate_range(*map(create_point, lines.split(' -> '))):
            yield point


def generate_diagram(input):
    maxx = max(x for x, y in input) + 1
    maxy = max(y for x, y in input) + 1
    diagram = np.zeros((maxy, maxx))
    for x, y in input:
        diagram[y, x] += 1
    return diagram


with open('input.txt') as io:
    input = io.read().split('\n')
    coordinates = list(generate_coordinates(input))
    diagram = generate_diagram(coordinates)
    print((diagram > 1).sum())

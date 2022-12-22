
from io import open


class Bound:
    def __init__(self, minx, maxx, miny, maxy) -> None:
        self.minx = minx
        self.maxx = maxx
        self.miny = miny
        self.maxy = maxy

    def contains(self, sand):
        return self.minx <= sand.x <= self.maxx and self.miny <= sand.y <= self.maxy


class InfBound:
    def __init__(self, maxy) -> None:
        self.maxy = maxy + 1

    def contains(self, sand):
        return sand.y < self.maxy


class Point:
    def __init__(self, x, y) -> None:
        self.x = x
        self.y = y

    def __hash__(self) -> int:
        return hash((self.x, self.y))

    def __eq__(self, o: object) -> bool:
        return self.x == o[0] and self.y == o[1]

    def __repr__(self) -> str:
        return repr((self.x, self.y))


class Rock(Point):
    pass


class Sand(Point):
    def down(self, block):
        if (self.x, self.y + 1) in block:
            return False

        self.y += 1
        return True

    def down_left(self, cave):
        if (self.x - 1, self.y + 1) in cave:
            return False

        self.x -= 1
        self.y += 1
        return True

    def down_right(self, cave):
        if (self.x + 1, self.y + 1) in cave:
            return False

        self.x += 1
        self.y += 1
        return True


def lmap(func, iterables):
    return list(map(func, iterables))


def minmax(array):
    minx = maxx = 500
    miny = maxy = 0
    for row in array:
        for column in row:
            x, y = column

            if x < minx:
                minx = x
            if x > maxx:
                maxx = x

            if y < miny:
                miny = y
            if y > maxy:
                maxy = y
    return minx, maxx, miny, maxy


def parseInput():
    with open('input.txt') as io:
        return lmap(lambda x: lmap(lambda n: tuple(map(int, n.split(','))), x.split(' -> ')), io.read().splitlines())


def createCave(structure):
    rocks = set()

    for paths in structure:
        for (x, y), (v, w) in zip(paths[:-1], paths[1:]):
            if v == x:
                start, stop = (y, w + 1) if y < w else (w, y + 1)
                rocks.update([(x, i) for i in range(start, stop)])
            else:
                start, stop = (x, v + 1) if x < v else (v, x + 1)
                rocks.update([(i, y) for i in range(start, stop)])

    return rocks


def part1():
    input = parseInput()
    bound = Bound(*minmax(input))
    cave = createCave(input)
    rest = set()

    while True:
        sand = Sand(500, 0)
        tiles = cave.union(rest)

        while bound.contains(sand):
            if not (sand.down(tiles) or sand.down_left(tiles) or sand.down_right(tiles)):
                break

        if bound.contains(sand):
            rest.add(sand)
        else:
            break

    print(f'Part 1: {len(rest)}')


def part2():
    input = parseInput()
    _, _, _, maxy = minmax(input)
    bound = InfBound(maxy)
    cave = createCave(input)
    rest = set()

    while True:
        sand = Sand(500, 0)
        tiles = cave.union(rest)

        while bound.contains(sand):
            if not (sand.down(tiles) or sand.down_left(tiles) or sand.down_right(tiles)):
                break

        rest.add(sand)

        if sand.x == 500 and sand.y == 0:
            break

    print(f'Part 2: {len(rest)}')


if __name__ == '__main__':
    part1()
    part2()

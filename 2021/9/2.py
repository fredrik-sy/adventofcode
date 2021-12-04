from io import open


def pop():
    while queue:
        x, y = queue.pop()
        if heightmap[y][x] != 9:
            return {(x, y)}
    return set()


def adjacent(x, y):
    if y != 0:
        yield x, y - 1
    if y != len(heightmap) - 1:
        yield x, y + 1
    if x != 0:
        yield x - 1, y
    if x != len(heightmap[0]) - 1:
        yield x + 1, y


with open('input.txt') as io:
    heightmap = [list(map(int, list(line))) for line in io.read().splitlines()]
    queue = {(x, y) for y in range(len(heightmap))
             for x in range(len(heightmap[y]))}
    sizes = []
    while queue:
        unexplored = pop()
        size = 0
        while unexplored:
            x, y = unexplored.pop()
            size += 1
            for m, n in adjacent(x, y):
                if (m, n) in queue and heightmap[n][m] != 9:
                    queue.remove((m, n))
                    unexplored.add((m, n))
        sizes.append(size)
    sizes.sort()
    print(sizes[-1] * sizes[-2] * sizes[-3])

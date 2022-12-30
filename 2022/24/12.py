from __future__ import annotations
from io import open


def lmap(func, iterables):
    return list(map(func, iterables))


def input():
    with open('input.txt') as io:
        return lmap(lambda line: list(line)[1:-1], io.read().splitlines())[1:-1]


def adjacent(x, y):
    return [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1), (x, y)]


def has_blizzard_or_wall(x, y, puzzle_map, minutes):
    if y < 0 or y >= len(puzzle_map):
        return True

    if x < 0 or x >= len(puzzle_map[y]):
        return True

    if puzzle_map[(y + minutes) % len(puzzle_map)][x] == '^':
        return True

    if puzzle_map[(y - minutes) % len(puzzle_map)][x] == 'v':
        return True

    if puzzle_map[y][(x + minutes) % len(puzzle_map[y])] == '<':
        return True

    if puzzle_map[y][(x - minutes) % len(puzzle_map[y])] == '>':
        return True

    return False


def search(start, stop, puzzle_map, minutes):
    open_set = {start}

    while True:
        next_set = set()

        for ox, oy in open_set:
            for x, y in adjacent(ox, oy):
                if (x, y) == stop:
                    return minutes

                if not has_blizzard_or_wall(x, y, puzzle_map, minutes):
                    next_set.add((x, y))

        open_set = next_set
        minutes += 1

        if not open_set:
            open_set.add(start)


def part1():
    puzzle_map = input()
    xy0 = (0, -1)
    xy = (len(puzzle_map[0]) - 1, len(puzzle_map))
    print(f'Part 1: {search(xy0, xy, puzzle_map, 1)}')


def part2():
    puzzle_map = input()
    xy0 = (0, -1)
    xy = (len(puzzle_map[0]) - 1, len(puzzle_map))
    start = search(xy0, xy, puzzle_map, 1)
    back = search(xy, xy0, puzzle_map, start)
    start = search(xy0, xy, puzzle_map, back)
    print(f'Part 2: {start}')


if __name__ == '__main__':
    part1()
    part2()

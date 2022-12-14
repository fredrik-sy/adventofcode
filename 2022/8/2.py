from io import open
from re import fullmatch
import sys


def left_scenic_score(grid, gx, gy):
    x = None
    for x in range(gx - 1, -1, -1):
        if grid[gy][gx] <= grid[gy][x]:
            return abs(gx - x)

    return 0 if x is None else abs(gx - x)


def right_scenic_score(grid, gx, gy):
    x = None
    for x in range(gx + 1, len(grid[gy])):
        if grid[gy][gx] <= grid[gy][x]:
            return abs(gx - x)

    return 0 if x is None else abs(gx - x)


def top_scenic_score(grid, gx, gy):
    y = None
    for y in range(gy - 1, -1, -1):
        if grid[gy][gx] <= grid[y][gx]:
            return abs(gy - y)

    return 0 if y is None else abs(gy - y)


def bottom_scenic_score(grid, gx, gy):
    y = None
    for y in range(gy + 1, len(grid)):
        if grid[gy][gx] <= grid[y][gx]:
            return abs(gy - y)

    return 0 if y is None else abs(gy - y)


def parse():
    with open('input.txt') as io:
        return list(map(lambda row: list(map(int, list(row))), io.read().split('\n')))


def part2(grid):
    highest_scenic_score = 0

    for y in range(len(grid)):
        for x in range(len(grid[y])):
            bottom_score = bottom_scenic_score(grid, x, y)
            left_score = left_scenic_score(grid, x, y)
            right_score = right_scenic_score(grid, x, y)
            top_score = top_scenic_score(grid, x, y)

            scenic_score = bottom_score * left_score * right_score * top_score
            highest_scenic_score = max(highest_scenic_score, scenic_score)

    print(f'Part 2 {highest_scenic_score}')


if __name__ == '__main__':
    grid = parse()
    part2(grid)

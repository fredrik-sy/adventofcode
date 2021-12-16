import heapq
import sys


def adjacent(x, y):
    if x - 1 >= 0:
        yield x - 1, y
    if x + 1 < len(level[y]):
        yield x + 1, y
    if y - 1 >= 0:
        yield x, y - 1
    if y + 1 < len(level):
        yield x, y + 1


with open('input.txt') as io:
    level = [list(map(int, line)) for line in io.read().splitlines()]
    open_set = [(0, 0, 0)]
    came_from = {}
    score = {(0, 0): 0}
    while open_set:
        _, x, y = heapq.heappop(open_set)

        if y == len(level) - 1 and x == len(level[y]) - 1:
            print(score[(x, y)])

        for nx, ny in adjacent(x, y):
            tentative_score = score[(x, y)] + level[ny][nx]
            if tentative_score < score.setdefault((nx, ny), sys.maxsize):
                came_from[(nx, ny)] = x, y
                score[(nx, ny)] = tentative_score
                if (nx, ny) not in open_set:
                    open_set.append((tentative_score, nx, ny))

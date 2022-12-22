
from queue import PriorityQueue
from io import open
import sys


def parse():
    with open('input.txt') as io:
        return list(map(list, io.read().split()))


def current_position(graph):
    for y in range(len(graph)):
        for x in range(len(graph[y])):
            if graph[y][x] == 'S':
                graph[y][x] = 'a'
                return x, y


def find_a(graph):
    for y in range(len(graph)):
        for x in range(len(graph[y])):
            if graph[y][x] == 'S' or graph[y][x] == 'a':
                graph[y][x] = 'a'
                yield x, y


def best_signal(graph):
    for y in range(len(graph)):
        for x in range(len(graph[y])):
            if graph[y][x] == 'E':
                graph[y][x] = 'z'
                return x, y


def neighbor(x, y, maxx, maxy, graph):
    for nx, ny in ((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)):
        if 0 <= nx < maxx and 0 <= ny < maxy:
            if ord(graph[ny][nx]) - ord(graph[y][x]) < 2:
                yield nx, ny


def part1():
    graph = parse()
    maxx = len(graph[0])
    maxy = len(graph)
    distance = [[sys.maxsize for _ in range(maxx)] for _ in range(maxy)]
    previous = [[None for _ in range(maxx)] for _ in range(maxy)]
    queue = PriorityQueue()
    sx, sy = best_signal(graph)
    px, py = current_position(graph)
    queue.put((0, (px, py)))
    distance[py][px] = 0

    while not queue.empty():
        _, (x, y) = queue.get()

        for nx, ny in neighbor(x, y, maxx, maxy, graph):
            alt = distance[y][x] + 1

            if alt < distance[ny][nx]:
                distance[ny][nx] = alt
                previous[ny][nx] = (x, y)
                queue.put((alt, (nx, ny)))

    print(f'Part 1: {distance[sy][sx]}')


def part2():
    graph = parse()
    maxx = len(graph[0])
    maxy = len(graph)
    distance = [[sys.maxsize for _ in range(maxx)] for _ in range(maxy)]
    previous = [[None for _ in range(maxx)] for _ in range(maxy)]
    queue = PriorityQueue()
    sx, sy = best_signal(graph)

    for px, py in find_a(graph):
        queue.put((0, (px, py)))
        distance[py][px] = 0

    while not queue.empty():
        _, (x, y) = queue.get()

        for nx, ny in neighbor(x, y, maxx, maxy, graph):
            alt = distance[y][x] + 1

            if alt < distance[ny][nx]:
                distance[ny][nx] = alt
                previous[ny][nx] = (x, y)
                queue.put((alt, (nx, ny)))

    print(f'Part 2: {distance[sy][sx]}')


if __name__ == '__main__':
    part1()
    part2()

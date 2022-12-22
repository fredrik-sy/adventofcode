
from collections import namedtuple
from io import open
from re import match

Beacon = namedtuple('Beacon', ['x', 'y'])
Point = namedtuple('Point', ['x', 'y'])
Sensor = namedtuple('Sensor', ['x', 'y'])


def lmap(func, iterables):
    return list(map(func, iterables))


def parseInput():
    report = set()
    with open('input.txt') as io:
        for x, y, n, m in lmap(lambda x: lmap(int, match('Sensor at x=([-0-9]+), y=([-0-9]+): closest beacon is at x=([-0-9]+), y=([-0-9]+)', x).groups()), io.read().splitlines()):
            report.add((Sensor(x, y), Beacon(n, m)))
    return report


def manhattan_distance(sensor: Sensor, point: Beacon | Point):
    return abs(sensor.x - point.x) + abs(sensor.y - point.y)


def intersection(sensor: Sensor, distance, y):
    if y <= sensor.y:
        minx = sensor.x + sensor.y - y - distance
        maxx = sensor.x - sensor.y + y + distance
    else:
        minx = sensor.x - sensor.y + y - distance
        maxx = sensor.x + sensor.y - y + distance
    return minx, maxx


def part1():
    report = parseInput()
    eliminated = []
    ignored = set()
    y = 2000000

    for sensor, beacon in report:
        beacon_distance = manhattan_distance(sensor, beacon)
        sensor_range = manhattan_distance(sensor, Point(sensor.x, y))

        if sensor_range <= beacon_distance:
            minx, maxx = intersection(sensor, beacon_distance, y)
            eliminated.append(minx)
            eliminated.append(maxx)

        if sensor.y == y:
            ignored.add(sensor.x)

        if beacon.y == y:
            ignored.add(beacon.x)

    eliminated.sort()
    print(f'Part 1: {eliminated[-1] - eliminated[0] + 1 - len(ignored)}')


def part2():
    report = parseInput()
    maxv = 4000000

    for y in range(0, maxv + 1):
        intervals = []

        for sensor, beacon in report:
            beacon_distance = manhattan_distance(sensor, beacon)
            sensor_range = manhattan_distance(sensor, Point(sensor.x, y))

            if sensor_range <= beacon_distance:
                intervals.append(intersection(sensor, beacon_distance, y))

        intervals.sort()
        x = 0
        for minx, maxx in intervals:
            if maxx < 0 or maxx < x:
                pass
            elif minx <= x <= maxx:
                x = maxx
            elif x <= maxv:
                print(f'Part 2: {(x + 1) * 4000000 + y}')
                exit()


if __name__ == '__main__':
    part1()
    part2()

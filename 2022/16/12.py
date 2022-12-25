
from collections import namedtuple
from dataclasses import dataclass
from multiprocessing import Pool
from queue import PriorityQueue
from re import findall
from sys import maxsize


Valve = namedtuple('Valve', ['flowrate', 'tunnels'])


@dataclass
class Path:
    distances: dict[str, int]
    shortest: dict[str, dict]


@dataclass
class Source:
    label: str
    pressure: int
    released: int
    minutes: int

    def copy(self):
        return Source(self.label, self.pressure, self.released, self.minutes)


def lmap(func, iterables):
    return list(map(func, iterables))


def digit(string: str):
    return int(string) if string.isdigit() else string


def parseInput() -> dict[str, Valve]:
    valves = {}

    with open('input.txt') as io:
        for label, flowrate, *tunnels in lmap(lambda line: lmap(digit, findall('[A-Z]{2}|[0-9]+', line)), io.read().splitlines()):
            valves[label] = Valve(flowrate, tunnels)

    return valves


def find_shortest_paths(valves: dict[str, Valve], close: set[str]):
    paths = {}

    for label in valves.keys():
        queue = PriorityQueue()
        queue.put((0, label))
        distances = {key: maxsize for key in valves.keys()}
        distances[label] = 0
        previous = {}

        while not queue.empty():
            _, key = queue.get()

            for neighbor in valves[key].tunnels:
                alt = distances[key] + 1

                if alt <= distances[neighbor]:
                    distances[neighbor] = alt
                    previous[neighbor] = key
                    queue.put((alt, neighbor))

        shortest = {}

        for close_label in close:
            if close_label == label:
                continue

            current = shortest
            path = []
            previous_label = close_label

            while previous_label and previous_label in previous:
                if previous_label in close:
                    path.append(previous_label)

                previous_label = previous.get(previous_label)

            for path_label in reversed(path):
                current = current.setdefault(path_label, {})

        distances = {key: value for key,
                     value in distances.items() if valves[key].flowrate}
        paths[label] = Path(distances, shortest)

    return paths


def find_most_pressure_process(sources: list[Source], close: set[str], valves: dict[str, Valve], paths: dict[str, Path]):
    source = sources.pop(0)
    path = paths[source.label]
    pool = Pool()
    args = []

    for label, cost in path.distances.items():
        if label not in close:
            continue

        minutes = source.minutes - cost - 1

        if minutes >= 0:
            next_sources = list(map(lambda source: source.copy(), sources))
            next_sources.append(Source(label, source.pressure + valves[label].flowrate, source.released + (
                source.minutes - minutes) * source.pressure, minutes))
            args.append(
                (next_sources, {key for key in close if key != label}, valves, paths))

        next_sources = list(map(lambda source: source.copy(), sources))
        next_sources.append(Source(
            label, source.pressure, source.released + source.minutes * source.pressure, 0))
        args.append((next_sources, close, valves, paths))

    return max([released for released in pool.starmap(find_most_pressure, args)])


def find_most_pressure(sources: list[Source], close: set[str], valves: dict[str, Valve], paths: dict[str, Path]):
    if len(close) == 0 or all(map(lambda source: source.minutes == 0, sources)):
        return sum(map(lambda source: source.pressure * source.minutes + source.released, sources))

    released = 0
    source = sources.pop(0)
    path = paths[source.label]

    for label, cost in path.distances.items():
        if label not in close:
            continue

        minutes = source.minutes - cost - 1

        if minutes >= 0:
            next_sources = list(map(lambda source: source.copy(), sources))
            next_sources.append(Source(label, source.pressure + valves[label].flowrate, source.released + (
                source.minutes - minutes) * source.pressure, minutes))
            released = max(released, find_most_pressure(
                next_sources, {key for key in close if key != label}, valves, paths))

        next_sources = list(map(lambda source: source.copy(), sources))
        next_sources.append(Source(
            label, source.pressure, source.released + source.minutes * source.pressure, 0))
        released = max(released, find_most_pressure(
            next_sources, close, valves, paths))

    return released


def part1():
    valves = parseInput()
    close = {key for key, value in valves.items() if value.flowrate}
    paths = find_shortest_paths(valves, close)

    pressure = find_most_pressure(
        [Source('AA', 0, 0, 30)], close, valves, paths)

    print(f'Part 1: {pressure}')


def part2():
    valves = parseInput()
    close = {key for key, value in valves.items() if value.flowrate}
    paths = find_shortest_paths(valves, close)

    pressure = find_most_pressure(
        [Source('AA', 0, 0, 26), Source('AA', 0, 0, 26)], close, valves, paths)

    print(f'Part 2: {pressure}')


if __name__ == '__main__':
    part1()
    part2()

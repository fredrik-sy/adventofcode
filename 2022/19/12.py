
from collections import defaultdict
from dataclasses import dataclass
from enum import IntEnum
from io import open
from math import ceil, prod
from re import findall


class Robot(IntEnum):
    ORE = 0
    CLAY = 1
    OBSIDIAN = 2
    GEODE = 3


@dataclass
class Price:
    ore: int
    clay: int
    obsidian: int


@dataclass
class Blueprint:
    id: int
    ore: Price
    clay: Price
    obsidian: Price
    geode: Price


@dataclass(eq=True, frozen=True)
class Bag:
    ore: int
    clay: int
    obsidian: int
    geode: int
    ore_robot: int
    clay_robot: int
    obsidian_robot: int
    geode_robot: int
    minutes: int


def lmap(func, iterables):
    return list(map(func, iterables))


def imap(iterables):
    return lmap(int, iterables)


def input():
    with open('input.txt') as io:
        return [Blueprint(blueprint[0],
                          Price(blueprint[1], 0, 0),
                          Price(blueprint[2], 0, 0),
                          Price(blueprint[3], blueprint[4], 0),
                          Price(blueprint[5], 0, blueprint[6]))
                for blueprint in lmap(imap, lmap(lambda line: findall('\d+', line), io.read().splitlines()))]


def next_bags(current: Bag, blueprint: Blueprint, max_robots):
    if current.ore_robot < max_robots[Robot.ORE]:
        minutes = max(
            ceil((blueprint.ore.ore - current.ore) / current.ore_robot), 0) + 1
        if minutes > 0 and current.minutes - minutes > 0:
            yield Bag(current.ore + current.ore_robot * minutes - blueprint.ore.ore,
                      current.clay + current.clay_robot * minutes,
                      current.obsidian + current.obsidian_robot * minutes,
                      current.geode + current.geode_robot * minutes,
                      current.ore_robot + 1,
                      current.clay_robot,
                      current.obsidian_robot,
                      current.geode_robot,
                      current.minutes - minutes)

    if current.clay_robot < max_robots[Robot.CLAY]:
        minutes = max(
            ceil((blueprint.clay.ore - current.ore) / current.ore_robot), 0) + 1
        if minutes > 0 and current.minutes - minutes > 0:
            yield Bag(current.ore + current.ore_robot * minutes - blueprint.clay.ore,
                      current.clay + current.clay_robot * minutes,
                      current.obsidian + current.obsidian_robot * minutes,
                      current.geode + current.geode_robot * minutes,
                      current.ore_robot,
                      current.clay_robot + 1,
                      current.obsidian_robot,
                      current.geode_robot,
                      current.minutes - minutes)

    if current.obsidian_robot < max_robots[Robot.OBSIDIAN] and current.clay_robot > 0:
        minutes = max(ceil((blueprint.obsidian.ore - current.ore) / current.ore_robot),
                      ceil((blueprint.obsidian.clay - current.clay) / current.clay_robot), 0) + 1
        if minutes > 0 and current.minutes - minutes > 0:
            yield Bag(current.ore + current.ore_robot * minutes - blueprint.obsidian.ore,
                      current.clay + current.clay_robot * minutes - blueprint.obsidian.clay,
                      current.obsidian + current.obsidian_robot * minutes,
                      current.geode + current.geode_robot * minutes,
                      current.ore_robot,
                      current.clay_robot,
                      current.obsidian_robot + 1,
                      current.geode_robot,
                      current.minutes - minutes)

    if current.geode_robot < max_robots[Robot.GEODE] and current.obsidian_robot > 0:
        minutes = max(ceil((blueprint.geode.ore - current.ore) / current.ore_robot),
                      ceil((blueprint.geode.obsidian - current.obsidian) / current.obsidian_robot), 0) + 1
        if minutes > 0 and current.minutes - minutes > 0:
            yield Bag(current.ore + current.ore_robot * minutes - blueprint.geode.ore,
                      current.clay + current.clay_robot * minutes,
                      current.obsidian + current.obsidian_robot * minutes - blueprint.geode.obsidian,
                      current.geode + current.geode_robot * minutes,
                      current.ore_robot,
                      current.clay_robot,
                      current.obsidian_robot,
                      current.geode_robot + 1,
                      current.minutes - minutes)

    if current.geode_robot > 0:
        yield Bag(current.ore + current.ore_robot * current.minutes,
                  current.clay + current.clay_robot * current.minutes,
                  current.obsidian + current.obsidian_robot * current.minutes,
                  current.geode + current.geode_robot * current.minutes,
                  current.ore_robot,
                  current.clay_robot,
                  current.obsidian_robot,
                  current.geode_robot,
                  0)


def search_best_quality(start: Bag, blueprint: Blueprint):
    open_queue = []
    open_queue.append(start)
    score = defaultdict(int)
    best_quality = start
    max_robots = {Robot.ORE: max(blueprint.ore.ore, blueprint.clay.ore, blueprint.obsidian.ore, blueprint.geode.ore),
                  Robot.CLAY: blueprint.obsidian.clay,
                  Robot.OBSIDIAN: blueprint.geode.obsidian,
                  Robot.GEODE: 100}

    while open_queue:
        current = open_queue.pop()
        score[current.minutes] = max(score[current.minutes], current.geode)

        if current.geode == score[current.minutes]:
            if current.minutes == 0:
                if current.geode > best_quality.geode:
                    best_quality = current
                continue

            for bag in next_bags(current, blueprint, max_robots):
                open_queue.append(bag)

    return best_quality


def part1():
    blueprints = input()
    quality_levels = [index * search_best_quality(Bag(0, 0, 0, 0, 1, 0, 0, 0, 24), blueprint).geode
                      for index, blueprint in enumerate(blueprints, 1)]
    print(f'Part 1: {sum(quality_levels)}')


def part2():
    blueprints = input()
    geodes = [search_best_quality(Bag(0, 0, 0, 0, 1, 0, 0, 0, 32), blueprint).geode
              for blueprint in blueprints[:3]]
    print(f'Part 2: {prod(geodes)}')


if __name__ == '__main__':
    part1()
    part2()


from dataclasses import dataclass
from io import open
from itertools import cycle
import numpy as np


@dataclass
class Rock:
    array: np.ndarray
    y: int

    @property
    def height(self):
        return self.array.shape[0]

    @property
    def width(self):
        return self.array.shape[1]


@dataclass
class Tower:
    array: np.ndarray
    fallen_rocks: int
    y: int

    @property
    def height(self):
        return self.array.shape[0]

    @property
    def width(self):
        return self.array.shape[1]


def input():
    with open('input.txt') as io:
        return cycle(list(io.read()))


def rocks_types():
    return cycle([
        np.array([[0, 0, 1, 1, 1, 1]]),
        np.array([[0, 0, 0, 1, 0],
                  [0, 0, 1, 1, 1],
                  [0, 0, 0, 1, 0]]),
        np.array([[0, 0, 0, 0, 1],
                  [0, 0, 0, 0, 1],
                  [0, 0, 1, 1, 1]]),
        np.array([[0, 0, 1],
                  [0, 0, 1],
                  [0, 0, 1],
                  [0, 0, 1]]),
        np.array([[0, 0, 1, 1],
                  [0, 0, 1, 1]])
    ])


def pad_left(rock: Rock, length):
    return Rock(np.hstack((np.zeros((rock.height, length), int), rock.array)), rock.y)


def pad_right(rock: Rock, length):
    return Rock(np.hstack((rock.array, np.zeros((rock.height, length), int))), rock.y)


def slice_left(rock: Rock, length):
    return Rock(rock.array[:, length:], rock.y)


def slice_right(rock: Rock, length):
    return Rock(rock.array[:, :rock.width-length], rock.y)


def push_left(tower: Tower, rock: Rock):
    if np.any(rock.array[:, 0] > 0):
        return rock

    pushed_rock = slice_left(rock, 1)

    if intersect(tower, pushed_rock):
        return rock

    return pushed_rock


def push_right(tower: Tower, rock: Rock):
    if rock.width == 7:
        return rock

    pushed_rock = pad_left(rock, 1)

    if intersect(tower, pushed_rock):
        return rock

    return pushed_rock


def fall(tower: Tower, rock: Rock):
    rock.y += 1

    if rock.y + rock.height > tower.height or intersect(tower, rock):
        rock.y -= 1
        return False

    return True


def intersect(tower: Tower, rock: Rock):
    return np.any(intersection(tower, rock) > 1)


def intersection(tower: Tower, rock: Rock):
    rock = pad_right(rock, 7 - rock.width)
    return tower.array[rock.y:rock.y+rock.height] + rock.array


def join(tower: Tower, rock: Rock):
    return Tower(np.concatenate((tower.array[:rock.y], intersection(tower, rock), tower.array[rock.y+rock.height:])),
                 tower.fallen_rocks + 1,
                 min(tower.y, rock.y))


def next_rock(tower: Tower, rocks):
    rock = next(rocks)
    rock = Rock(rock, tower.y - rock.shape[0] - 3)
    return rock


def repeat_cycle_height(tower: Tower, count_rocks: dict[int, int]):
    length = 30

    for y in range(tower.height, tower.y, -1):
        search = tower.array[y - length:y]

        for i in range(y - 1, tower.y, -1):
            if np.array_equal(search, tower.array[i - length:i]):
                for y in range(i - 1, tower.y, -1):
                    if np.array_equal(search, tower.array[y - length:y]):
                        height = i - y
                        fallen_rocks = sum(count_rocks.get(n, 0) for n in range(y, i))
                        repeat = int((1000000000000 - tower.fallen_rocks) / fallen_rocks)
                        tower.fallen_rocks += repeat * fallen_rocks
                        return repeat * height


def part1():
    jets = input()
    tower = Tower(np.zeros((10000, 7), dtype=int), 0, 10000)
    rocks = rocks_types()

    rock = next_rock(tower, rocks)

    while tower.fallen_rocks < 2022:
        if next(jets) == '>':
            rock = push_right(tower, rock)
        else:
            rock = push_left(tower, rock)

        if not fall(tower, rock):
            tower = join(tower, rock)
            rock = next_rock(tower, rocks)

    print(f'Part 1: {tower.height - tower.y}')


def part2():
    jets = input()
    tower = Tower(np.zeros((100000, 7), dtype=int), 0, 100000)
    count_rocks = {}
    rocks = rocks_types()
    bonus_height = 0

    rock = next_rock(tower, rocks)

    while tower.fallen_rocks < 1000000000000:
        if next(jets) == '>':
            rock = push_right(tower, rock)
        else:
            rock = push_left(tower, rock)

        if not fall(tower, rock):
            tower = join(tower, rock)
            rock = next_rock(tower, rocks)
            count_rocks.setdefault(rock.y, 0)
            count_rocks[rock.y] += 1

            if tower.y <= 2000 and bonus_height == 0:
                bonus_height = repeat_cycle_height(tower, count_rocks)

    print(f'Part 2: {tower.height - tower.y + bonus_height}')


if __name__ == '__main__':
    part1()
    part2()

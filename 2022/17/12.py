
from io import open
from queue import Queue
import numpy as np


class Rock:
    def __init__(self, array, y) -> None:
        self.array = array
        self.y = y

    def height(self, array=None):
        return self.array.shape[0] if array is None else array.shape[0]

    def width(self, array=None):
        return self.array.shape[1] if array is None else array.shape[1]

    def pad_left(self, array, size):
        return np.hstack((np.zeros((self.height(array), size), int), array))

    def pad_right(self, array, size):
        return np.hstack((array, np.zeros((self.height(array), size), int)))

    def slice_left(self):
        return self.array[:, 1:]

    def merge_towerrock(self, tower, rock, y):
        tower_height = self.height(tower)

        if tower_height == 0:
            tower = np.zeros((1, 7), int)

        array = self.pad_right(rock, 7 - self.width())
        maxy = max(0, y - self.height(array))
        miny = max(-y, -self.height(array))
        towerrock = np.concatenate(
            (array[0:self.height(array)+miny], tower[maxy:y] + array[np.r_[miny:0]]))
        return np.concatenate((tower[0:maxy], towerrock, tower[np.r_[y-tower_height:0]]))

    def push(self, tower, rock):
        miny = max(0, -self.y - self.height())
        maxy = max(self.y, -self.height(rock))
        rock = self.pad_right(rock, 7 - self.width(rock))
        towerrock = tower[miny:-self.y] + rock[np.r_[maxy:0]]
        return np.all(towerrock <= 1)

    def push_left(self, tower):
        if np.any(self.array[:, 0] > 0):
            return

        array = self.slice_left()

        if self.is_free_fall():
            self.array = array
            return

        if self.push(tower, array):
            self.array = array

    def push_right(self, tower):
        if self.width() == 7:
            return

        array = self.pad_left(self.array, 1)

        if self.is_free_fall():
            self.array = array
            return

        if self.push(tower, array):
            self.array = array

    def is_free_fall(self):
        return self.y > 0

    def fall(self, tower):
        if self.is_free_fall():
            self.y -= 1
            return True

        y = abs(self.y - 1)
        towerrock = self.merge_towerrock(tower, self.array, y)

        if np.any(towerrock > 1) or self.height(tower) <= y:
            return False

        self.y -= 1
        return True

    def rest(self, tower):
        return self.merge_towerrock(tower, self.array, abs(self.y))


class Chamber:
    def __init__(self) -> None:
        self.fallen_rocks = 0
        self.rock = None
        self.rocks = Queue()
        self.rocks.put(np.array([[0, 0, 1, 1, 1, 1]]))
        self.rocks.put(np.array([[0, 0, 0, 1, 0],
                                 [0, 0, 1, 1, 1],
                                 [0, 0, 0, 1, 0]]))
        self.rocks.put(np.array([[0, 0, 0, 0, 1],
                                 [0, 0, 0, 0, 1],
                                 [0, 0, 1, 1, 1]]))
        self.rocks.put(np.array([[0, 0, 1],
                                 [0, 0, 1],
                                 [0, 0, 1],
                                 [0, 0, 1]]))
        self.rocks.put(np.array([[0, 0, 1, 1],
                                 [0, 0, 1, 1]]))
        self.tower = np.zeros((0, 7), dtype=int)

    def height(self):
        return self.tower.shape[0]

    def width(self):
        return self.tower.shape[1]

    def update(self, direction):
        if not self.rock:
            array = self.rocks.get()
            self.rocks.put(array)
            self.rock = Rock(array, 3)

        if direction == '>':
            self.rock.push_right(self.tower)
        else:
            self.rock.push_left(self.tower)

        if not self.rock.fall(self.tower):
            self.tower = self.rock.rest(self.tower)
            self.rock = None
            self.fallen_rocks += 1

        if self.has_cycle():
            print(self.fallen_rocks)
            exit(0)

    def has_cycle(self):
        height = self.height()

        if height < 120:
            return False

        seek = self.tower[height-10:height]

        for y in range(10, height-10):
            lookup = self.tower[height-10-y:height-y]

            if np.array_equal(seek, lookup):
                return True

        return False


def lmap(func, iterables):
    return list(map(func, iterables))


def parseInput():
    with open('input.txt') as io:
        return list(io.read())


def part1():
    jets = parseInput()
    chamber = Chamber()
    i = 0

    while chamber.fallen_rocks <= 1000000000000:
        chamber.update(jets[i % len(jets)])
        i += 1

    print(f'Part 1: {chamber.height()}')


def part2():
    jets = parseInput()


if __name__ == '__main__':
    part1()
    part2()

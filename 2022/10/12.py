from collections import deque
from io import open
import re


def parse():
    with open('input.txt') as io:
        for line in io.read().split('\n'):
            match = re.match('addx (.*)', line)

            if match:
                yield None, None
                yield 'addx', int(match.groups()[0])
            else:
                yield 'noop', None


def part1():
    register = 1
    signal_strengths = []

    for cycle, (instruction, value) in enumerate(list(parse()), 1):
        if cycle in [20, 60, 100, 140, 180, 220]:
            signal_strengths.append(cycle * register)

        if instruction == 'addx':
            register += value

    print(f'Part 1 {sum(signal_strengths)}')


def part2():
    crt = []
    sprite = deque('###.....................................')

    for cycle, (instruction, value) in enumerate(list(parse()), 1):
        crt.append(sprite[(cycle - 1) % 40])

        if instruction == 'addx':
            sprite.rotate(value)

    print(f'Part 2')
    for i in range(6):
        print(''.join(crt[i * 40: i * 40 + 40]))


if __name__ == '__main__':
    part1()
    part2()

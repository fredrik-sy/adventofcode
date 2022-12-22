from collections import deque
from functools import reduce
from io import open
from itertools import accumulate
from math import gcd
import re


class Monkey:
    def init(self, data: list):
        self.false = data.pop()
        self.true = data.pop()
        self.mod = data.pop()
        self.val = data.pop()
        self.mul = data.pop() == '*'
        self.op = ((lambda x: x * self.val) if isinstance(self.val, int) else (lambda x: x * x)
                   ) if self.mul else ((lambda x: x + self.val) if isinstance(self.val, int) else (lambda x: x + x))
        data.pop()
        self.items = data

    def test(self, x):
        return self.true if x % self.mod == 0 else self.false

    def operation(self, x):
        return self.op(x)


def parse():
    with open('input.txt') as io:
        input = io.read().split('\n\n')
        monkeys = [Monkey() for _ in input]

        for monkey in input:
            data = list(map(lambda m: int(m) if m.isdigit()
                        else m, re.findall('old|\+|\*|\d+', monkey)))
            monkeys[data[0]].init(data[1:])

        return monkeys


def part1():
    monkeys = parse()
    times = [0] * len(monkeys)

    for round in range(20):
        for index, monkey in enumerate(monkeys):
            for item in monkey.items:
                worry_level = int(monkey.operation(item) / 3)
                throw_to_monkey = monkey.test(worry_level)
                monkeys[throw_to_monkey].items.append(worry_level)
                times[index] += 1

            monkey.items.clear()

    times.sort()
    print(f'Part 1 {times[-1] * times[-2]}')


def part2():
    monkeys = parse()
    times = [0] * len(monkeys)
    divider = reduce(lambda x, y: x * y, [monkey.mod for monkey in monkeys])

    for round in range(10000):
        for index, monkey in enumerate(monkeys):
            for item in monkey.items:
                worry_level = monkey.operation(item) % divider
                throw_to_monkey = monkey.test(worry_level)
                monkeys[throw_to_monkey].items.append(worry_level)
                times[index] += 1

            monkey.items.clear()

    times.sort()
    print(f'Part 2 {times[-1] * times[-2]}')


if __name__ == '__main__':
    part1()
    part2()

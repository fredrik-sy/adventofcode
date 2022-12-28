from __future__ import annotations
from dataclasses import dataclass
from io import open
from queue import Queue
from re import findall


@dataclass
class Integer:
    value: int

    def __call__(self):
        return self.value

    def __iter__(self):
        return iter([])


class Monkey:
    def __init__(self, monkeys, monkey0, op, monkey1) -> None:
        self.monkeys = monkeys
        self.monkey0 = monkey0
        self.op = op
        self.monkey1 = monkey1

    def __call__(self):
        if self.op == '*':
            return self.monkeys[self.monkey0]() * self.monkeys[self.monkey1]()
        elif self.op == '/':
            return int(self.monkeys[self.monkey0]() / self.monkeys[self.monkey1]())
        elif self.op == '+':
            return self.monkeys[self.monkey0]() + self.monkeys[self.monkey1]()
        else:
            return self.monkeys[self.monkey0]() - self.monkeys[self.monkey1]()

    def __iter__(self):
        return iter([self.monkey0, self.monkey1])


def input():
    monkeys = {}

    with open('input.txt') as io:
        for line in io.read().splitlines():
            match = findall('[a-z0-9*/+-]+', line)

            if len(match) == 2:
                name, num = match
                monkeys[name] = Integer(int(num))
            else:
                name, monkey0, op, monkey1 = match
                monkeys[name] = Monkey(monkeys, monkey0, op, monkey1)

    return monkeys


def find_humn(monkeys: dict[str, Monkey | Integer]):
    q = Queue()
    q.put('root')
    explored = {}

    while not q.empty():
        current = q.get()

        if current == 'humn':
            path = [current]

            while current in explored:
                current = explored[current]
                path.append(current)

            return path

        for name in monkeys[current]:
            if name not in explored:
                explored[name] = current
                q.put(name)


def reverse(monkeys: dict[str, Monkey | Integer], path: list[str]):
    head = monkeys[path.pop()]
    monkey = path.pop()
    rname = head.monkey1 if head.monkey0 == monkey else head.monkey0
    rmonkey = monkeys[rname]

    for next in reversed(path):
        current = monkeys[monkey]
        op = '/' if current.op == '*' else '*' if current.op == '/' else '-' if current.op == '+' else '+'

        if current.monkey0 == next:
            rmonkey = Monkey(monkeys, rname, op, current.monkey1)
        else:
            if current.op == '-':
                rmonkey = Monkey(monkeys, current.monkey0, '-', rname)
            else:
                rmonkey = Monkey(monkeys, rname, op, current.monkey0)

        monkeys[monkey] = rmonkey
        rname = monkey
        monkey = next

    return rname


def part1():
    monkeys = input()
    print(f'Part 1: {monkeys["root"]()}')


def part2():
    monkeys = input()
    path = find_humn(monkeys)
    print(f'Part 2: {monkeys[reverse(monkeys, list(path))]()}')


if __name__ == '__main__':
    part1()
    part2()

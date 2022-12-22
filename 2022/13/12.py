
from io import open
from itertools import chain


def parseInput():
    with open('input.txt') as io:
        return list(map(lambda x: list(map(lambda n: eval(n), x.splitlines())), io.read().split('\n\n')))


def cmp_number(a, b):
    return None if a == b else a < b


def cmp_list(a, b):
    for i in range(min(len(a), len(b))):
        c = cmp(a[i], b[i])
        if c is not None:
            return c

    if len(a) == len(b):
        return None

    if len(a) > len(b):
        return False

    return True


def cmp(a, b):
    if isinstance(a, list):
        if not isinstance(b, list):
            b = [b]
    else:
        if not isinstance(b, list):
            return cmp_number(a, b)

        a = [a]

    return cmp_list(a, b)


def part1():
    packets = parseInput()
    total = 0

    for index, (packet0, packet1) in enumerate(packets, 1):
        if cmp(packet0, packet1) is True:
            total += index

    print(f'Part 1: {total}')


def part2():
    packets = list(chain.from_iterable(parseInput()))
    divider0 = [[2]]
    divider1 = [[6]]
    packets.append(divider0)
    packets.append(divider1)
    ordered = False

    while not ordered:
        ordered = True

        for i in range(len(packets) - 1):
            if cmp(packets[i], packets[i + 1]) is False:
                temp = packets[i]
                packets[i] = packets[i + 1]
                packets[i + 1] = temp
                ordered = False

    print(f'Part 2: {(packets.index(divider0) + 1) * (packets.index(divider1) + 1)}')


if __name__ == '__main__':
    part1()
    part2()

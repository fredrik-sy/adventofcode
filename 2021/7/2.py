import sys


with open('input.txt') as io:
    positions = list(map(int, io.read().split(',')))
    horizontals = set(positions)
    min = sys.maxsize
    for horizontal in range(max(horizontals)):
        current = 0
        for position in positions:
            current += sum(range(abs(horizontal - position) + 1))
        if min > current:
            min = current
    print(min)


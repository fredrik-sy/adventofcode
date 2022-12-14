from io import open
import math


class Node:
    def __init__(self, max_size, size=0) -> None:
        self.x = 0
        self.y = 0
        self.child = Node(max_size, size + 1) if size < max_size else None
        self.visited = {(0, 0)}

    def propagate(self):
        self.child.move(self.x, self.y)

    def move(self, x, y):
        dx = x - self.x
        dy = y - self.y
        d = int(math.sqrt(pow(abs(dx), 2) + pow(abs(dy), 2)))

        if d >= 2:
            self.x += int(dx / 2) if abs(dx) == 2 else dx
            self.y += int(dy / 2) if abs(dy) == 2 else dy
            self.visited.add((self.x, self.y))

            if self.child != None:
                self.child.move(self.x, self.y)

    def tail(self):
        tail = self.child
        while tail.child:
            tail = tail.child
        return tail


def parse():
    with open('input.txt') as io:
        return list(map(lambda row: list(map(lambda col: int(col) if col.isdigit() else col, row.split())), io.read().split('\n')))


def part2(moves):
    head = Node(9)
    for direction, steps in moves:
        for _ in range(steps):
            if direction == 'R':
                head.x += 1
            elif direction == 'L':
                head.x -= 1
            elif direction == 'U':
                head.y -= 1
            else:
                head.y += 1
            head.propagate()

    print(f'Part 2 {len(head.tail().visited)}')


if __name__ == '__main__':
    moves = parse()
    part2(moves)

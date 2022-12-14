from io import open
import math


class Head:
    def __init__(self) -> None:
        self.x = 0
        self.y = 0
        self.prev_x = self.x
        self.prev_y = self.y
        self.tail = Tail()

    def move_r(self, steps):
        for _ in range(steps):
            self.prev_x = self.x
            self.prev_y = self.y
            self.x += 1
            print(f'Head: ({self.x}, {self.y})')
            self.tail.update(self)

    def move_l(self, steps):
        for _ in range(steps):
            self.prev_x = self.x
            self.prev_y = self.y
            self.x -= 1
            print(f'Head: ({self.x}, {self.y})')
            self.tail.update(self)

    def move_u(self, steps):
        for _ in range(steps):
            self.prev_x = self.x
            self.prev_y = self.y
            self.y -= 1
            print(f'Head: ({self.x}, {self.y})')
            self.tail.update(self)

    def move_d(self, steps):
        for _ in range(steps):
            self.prev_x = self.x
            self.prev_y = self.y
            self.y += 1
            print(f'Head: ({self.x}, {self.y})')
            self.tail.update(self)


class Tail:
    def __init__(self) -> None:
        self.x = 0
        self.y = 0
        self.visited = {(0, 0)}

    def update(self, head: Head):
        distance_x = abs(self.x - head.x)
        distance_y = abs(self.y - head.y)

        if distance_x > 1 or distance_y > 1 or distance_x + distance_y > 2:
            self.x = head.prev_x
            self.y = head.prev_y
            print(f'Tail: ({self.x}, {self.y})')
            self.visited.add((self.x, self.y))


def parse():
    with open('input.txt') as io:
        return list(map(lambda row: list(map(lambda col: int(col) if col.isdigit() else col, row.split())), io.read().split('\n')))


def part1(moves):
    head = Head()
    for direction, steps in moves:
        if direction == 'R':
            head.move_r(steps)
        elif direction == 'L':
            head.move_l(steps)
        elif direction == 'U':
            head.move_u(steps)
        else:
            head.move_d(steps)

    print(f'Part 1 {len(head.tail.visited)}')


if __name__ == '__main__':
    moves = parse()
    part1(moves)

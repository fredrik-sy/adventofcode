from __future__ import annotations
from dataclasses import dataclass
from enum import IntEnum
from io import open
from re import findall
from nptyping import NDArray, Object
import numpy as np


class Direction(IntEnum):
    LEFT = 2,
    RIGHT = 0,
    UP = 3,
    DOWN = 1


class Tile:
    letter: str
    up: Tile
    down: Tile
    left: Tile
    right: Tile
    row: int
    column: int

    def __init__(self, letter, row, column) -> None:
        self.letter = letter
        self.row = row
        self.column = column

    def __repr__(self) -> str:
        return f"'{self.letter}'"


def lmap(func, iterables):
    return list(map(func, iterables))


def digit(string: str):
    return int(string) if string.isdigit() else string


def connect_y(board):
    tiles: dict[int, list[Tile]] = {}

    for y in range(len(board)):
        for x in range(len(board[y])):
            if board[y][x] != ' ':
                tiles.setdefault(x, []).append(board[y][x])

    for column in tiles.values():
        for y in range(len(column)):
            column[y].up = column[(y - 1) % len(column)]
            column[y].down = column[(y + 1) % len(column)]


def connect_x(board: list[list[Tile]]):
    for y in range(len(board)):
        tiles = [tile for tile in board[y] if tile != ' ']

        for x in range(len(tiles)):
            tiles[x].right = tiles[(x + 1) % len(tiles)]
            tiles[x].left = tiles[(x - 1) % len(tiles)]


def connect_cube(board: list[list[Tile]]):
    size = 50
    cube = {}

    for y in range(0, len(board), size):
        for x in range(0, len(board[y]), size):
            for m in range(y, y + size):
                for n in range(x, x + size):
                    cube.setdefault((y, x), np.zeros((50, 50), object))[
                        m % size, n % size] = board[m][n]

    # _ 1 2
    # _ 3 _
    # 4 5 _
    # 6 _ _

    one = (0, 50)
    two = (0, 100)
    three = (50, 50)
    four = (100, 0)
    five = (100, 50)
    six = (150, 0)

    cube_one = cube[one][0]
    cube_six = cube[six][:, 0]
    for i in range(len(cube_one)):
        cube_one[i].up = cube_six[i]
        cube_six[i].left = cube_one[i]

    cube_one = cube[one][:, 0][::-1]
    cube_four = cube[four][:, 0]
    for i in range(len(cube_one)):
        cube_one[i].left = cube_four[i]
        cube_four[i].left = cube_one[i]

    cube_two = cube[two][0]
    cube_six = cube[six][-1]
    for i in range(len(cube_two)):
        cube_two[i].up = cube_six[i]
        cube_six[i].down = cube_two[i]

    cube_two = cube[two][:, -1][::-1]
    cube_five = cube[five][:, -1]
    for i in range(len(cube_two)):
        cube_two[i].right = cube_five[i]
        cube_five[i].right = cube_two[i]

    cube_two = cube[two][-1]
    cube_three = cube[three][:, -1]
    for i in range(len(cube_two)):
        cube_two[i].down = cube_three[i]
        cube_three[i].right = cube_two[i]

    cube_three = cube[three][:, 0]
    cube_four = cube[four][0]
    for i in range(len(cube_three)):
        cube_three[i].left = cube_four[i]
        cube_four[i].up = cube_four[i]

    cube_five = cube[five][-1]
    cube_six = cube[six][:, -1]
    for i in range(len(cube_five)):
        cube_five[i].down = cube_six[i]
        cube_six[i].right = cube_five[i]


def input():
    with open('input.txt') as io:
        board, followpath = io.read().split('\n\n')
        board = lmap(list, board.splitlines())

        for y in range(len(board)):
            for x in range(len(board[y])):
                if board[y][x] != ' ':
                    board[y][x] = Tile(board[y][x], y + 1, x + 1)

        return board, list(map(digit, findall('\d+|R|L', followpath)))


def start_tile(board: list[list[str | Tile]]):
    for y in range(len(board)):
        for x in range(len(board[y])):
            if board[y][x] != ' ' and board[y][x].letter != '#':
                return board[y][x]


def follow_instruction(board, followpath):
    tile = start_tile(board)
    direction = Direction.RIGHT

    for instruction in followpath:
        if direction == Direction.LEFT:
            if instruction == 'R':
                direction = Direction.UP
            elif instruction == 'L':
                direction = Direction.DOWN
            else:
                for _ in range(instruction):
                    if tile.left.letter == '.':
                        tile = tile.left
        elif direction == Direction.RIGHT:
            if instruction == 'R':
                direction = Direction.DOWN
            elif instruction == 'L':
                direction = Direction.UP
            else:
                for _ in range(instruction):
                    if tile.right.letter == '.':
                        tile = tile.right
        elif direction == Direction.UP:
            if instruction == 'R':
                direction = Direction.RIGHT
            elif instruction == 'L':
                direction = Direction.LEFT
            else:
                for _ in range(instruction):
                    if tile.up.letter == '.':
                        tile = tile.up
        else:
            if instruction == 'R':
                direction = Direction.LEFT
            elif instruction == 'L':
                direction = Direction.RIGHT
            else:
                for _ in range(instruction):
                    if tile.down.letter == '.':
                        tile = tile.down

    return tile, direction


def part1():
    board, followpath = input()
    connect_x(board)
    connect_y(board)
    tile, direction = follow_instruction(board, followpath)
    print(f'Part 1: {1000 * tile.row + 4 * tile.column + direction}')


def part2():
    board, followpath = input()
    connect_x(board)
    connect_y(board)
    connect_cube(board)
    tile, direction = follow_instruction(board, followpath)
    print(f'Part 2: {1000 * tile.row + 4 * tile.column + direction}')


if __name__ == '__main__':
    part1()
    part2()

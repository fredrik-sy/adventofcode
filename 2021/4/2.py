from io import open


class Board():
    def __init__(self, columns, rows):
        self.columns = columns
        self.rows = rows

    def __iter__(self):
        for column in self.columns:
            yield column
        for row in self.rows:
            yield row

    def unmarked(self, drawns):
        unmarked = set()
        for column in self.columns:
            unmarked.update(column - drawns)
        return unmarked


def parse(string: str):
    board = [list(map(int, row)) for row in map(str.split, string.split('\n'))]
    return Board(list(set(board[y][x] for y in range(5)) for x in range(5)),
                 list(set(board[y]) for y in range(5)))


with open('input.txt') as io:
    input = io.read().split('\n\n')
    numbers = list(map(int, input[0].split(',')))
    boards = list(combination for combination in map(parse, input[1:]))
    drawns = set()

    for drawn in numbers:
        drawns.add(drawn)
        for board in reversed(boards):
            for combination in board:
                if combination.issubset(drawns):
                    boards.remove(board)
                    if not boards:
                        unmarked = board.unmarked(drawns)
                        print(drawn * sum(unmarked))
                        exit()
                    break

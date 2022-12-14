from io import open


def is_left_visible(grid, gx, gy):
    for x in range(gx - 1, -1, -1):
        if grid[gy][gx] <= grid[gy][x]:
            return False

    return True


def is_right_visible(grid, gx, gy):
    for x in range(gx + 1, len(grid[gy])):
        if grid[gy][gx] <= grid[gy][x]:
            return False

    return True


def is_top_visible(grid, gx, gy):
    for y in range(gy - 1, -1, -1):
        if grid[gy][gx] <= grid[y][gx]:
            return False

    return True


def is_bottom_visible(grid, gx, gy):
    for y in range(gy + 1, len(grid)):
        if grid[gy][gx] <= grid[y][gx]:
            return False

    return True


def parse():
    with open('input.txt') as io:
        return list(map(lambda row: list(map(int, list(row))), io.read().split('\n')))


def part1(grid: list[list[str]]):
    visible = 0

    for y in range(len(grid)):
        for x in range(len(grid[y])):
            if is_bottom_visible(grid, x, y) or is_left_visible(grid, x, y) or is_right_visible(grid, x, y) or is_top_visible(grid, x, y):
                visible += 1

    print(f'Part 1 {visible}')


if __name__ == '__main__':
    grid = parse()
    part1(grid)

import numpy as np


def find_collision(direction):
    mask = np.transpose(np.where(sea_map == direction))
    if direction == '>':
        return {(x, y) for y, x in mask
                if sea_map[y, (x + 1) % width] == '>'
                or sea_map[y, (x + 1) % width] == 'v'}
    else:
        return {(x, y) for y, x in mask if sea_map[(y + 1) % height, x] == 'v'}


with open('input.txt') as io:
    sea_map = np.array([list(line) for line in io.read().splitlines()])
    height, width = sea_map.shape
    step = 0
    while True:
        next_map = np.full_like(sea_map, '.')
        collision = set().union(find_collision('>'), find_collision('v'))
        for y, x in np.transpose(np.where(sea_map == '>')):
            if (x, y) not in collision:
                next_map[y, (x + 1) % width] = '>'
            else:
                next_map[y, x] = '>'
        for y, x in np.transpose(np.where(sea_map == 'v')):
            ny = (y + 1) % height
            if (x, y) not in collision and next_map[ny, x] == '.':
                next_map[ny, x] = 'v'
            else:
                next_map[y, x] = 'v'
        step += 1
        if np.array_equal(sea_map, next_map):
            break
        sea_map = next_map
    print(step)

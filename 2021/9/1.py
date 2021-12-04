from io import open

with open('input.txt') as io:
    heightmap = [list(map(int, list(line))) for line in io.read().splitlines()]
    sum = 0
    for y in range(len(heightmap)):
        for x in range(len(heightmap[y])):
            up = down = left = right = True
            if y != 0:
                up = heightmap[y][x] < heightmap[y - 1][x]
            if y != len(heightmap) - 1:
                down = heightmap[y][x] < heightmap[y + 1][x]
            if x != 0:
                left = heightmap[y][x] < heightmap[y][x - 1]
            if x != len(heightmap[y]) - 1:
                right = heightmap[y][x] < heightmap[y][x + 1]
            if up and down and left and right:
                sum += heightmap[y][x] + 1
    print(sum)

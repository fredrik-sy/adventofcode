import re
import numpy as np

with open('input.txt') as io:
    dots, fold = io.read().split('\n\n')
    coordinates = [list(map(int, value.split(','))) for value in dots.splitlines()]
    maxx = max(coordinates, key=lambda coordinate: coordinate[0])[0] + 1
    maxy = max(coordinates, key=lambda coordinate: coordinate[1])[1] + 1
    paper = np.zeros((maxy, maxx), int)
    for x, y in coordinates:
        paper[y][x] = 1
    for match in re.finditer(r'fold along (x|y)=(\d+)', fold):
        if match.group(1) == 'x':
            x = int(match.group(2))
            paper = paper[:, :x] | np.fliplr(paper[:, x + 1:maxx])
        else:
            y = int(match.group(2))
            paper = paper[:y, :] | np.flipud(paper[y + 1:maxy, :])
        print(np.count_nonzero(paper))
        break

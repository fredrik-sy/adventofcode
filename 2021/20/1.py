class Bounds:
    def __init__(self, image) -> None:
        self.infinitebit = False
        self.miny = 0
        self.maxy = len(image)
        self.minx = 0
        self.maxx = len(image[0])

    def __contains__(self, item):
        x, y = item
        return self.minx <= x < self.maxx and self.miny <= y < self.maxy

    def update(self):
        self.infinitebit = not self.infinitebit
        self.minx -= 1
        self.miny -= 1
        self.maxx += 1
        self.maxy += 1


def enhancement_algorithm(x, y):
    binary = []
    for yn in range(y - 1, y + 2):
        for xn in range(x - 1, x + 2):
            if (xn, yn) in bounds:
                binary.append('1' if (xn, yn) in input else '0')
            else:
                binary.append(str(int(bounds.infinitebit)))
    return enhancement[int(''.join(binary), 2)]


with open('input.txt') as io:
    enhancement, image = io.read().split('\n\n')
    image = [list(line) for line in image.splitlines()]
    input = set()
    output = set()
    for y in range(len(image)):
        for x in range(len(image[y])):
            if image[y][x] == '#':
                input.add((x, y))
    bounds = Bounds(image)
    loop = 2
    while loop:
        for y in range(bounds.miny - 1, bounds.maxy + 1):
            for x in range(bounds.minx - 1, bounds.maxx + 1):
                if enhancement_algorithm(x, y) == '#':
                    output.add((x, y))
        input.clear()
        input.update(output)
        output.clear()
        bounds.update()
        loop -= 1
    print(len(input))

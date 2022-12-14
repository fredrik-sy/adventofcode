from io import open
from re import fullmatch
import sys


class File:
    def __init__(self, name, size) -> None:
        self.name = name
        self.size = size


class Directory:
    def __init__(self, name, parent) -> None:
        self.children = {}
        self.files = []
        self.name = name
        self.parent = parent

    def size(self):
        return sum(map(lambda child: child.size(), self.children.values())) + sum(map(lambda file: file.size, self.files))


def parse():
    with open('input.txt') as io:
        commands = list(reversed(io.read().split('\n')))

    root = Directory("/", None)
    cwd: Directory = root

    while (commands):
        command = commands.pop()
        if fullmatch("\$ cd /", command):
            cwd = root
            continue

        if fullmatch("\$ cd ..", command):
            cwd = cwd.parent
            continue

        match = fullmatch("\$ cd (\w+)", command)
        if match:
            (name,) = match.groups()
            cwd = cwd.children[name]
            continue

        if fullmatch("\$ ls", command):
            continue

        match = fullmatch("dir ([a-z]+)", command)
        if match:
            (name,) = match.groups()
            cwd.children[name] = Directory(name, cwd)
            continue

        match = fullmatch("([0-9]+) ([a-z.]+)", command)
        if match:
            (size, name) = match.groups()
            cwd.files.append(File(name, int(size)))
            continue

    return root


def findDirectories(directories: list[Directory], minSize: int, maxSize: int) -> list[Directory]:
    for directory in directories:
        if minSize <= directory.size() <= maxSize:
            yield directory
        yield from findDirectories(directory.children.values(), minSize, maxSize)


def part1(root: Directory):
    directories = list(findDirectories([root], 0, 100000))
    print(f'Part 1 {sum(map(lambda directory: directory.size(), directories))}')


def part2(root: Directory):
    totalSize = 70000000
    leastUnusedSize = 30000000
    size = root.size()
    targetSize = (size + leastUnusedSize) - totalSize
    directories = list(findDirectories([root], targetSize, sys.maxsize))
    print(f'Part 2 {min(map(lambda directory: directory.size(), directories))}')


if __name__ == '__main__':
    root = parse()
    part1(root)
    part2(root)

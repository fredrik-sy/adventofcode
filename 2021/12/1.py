from io import open
from os import path
from typing import Set

paths = set()


def search(explored: list, v: str):
    if v.islower() and v in explored:
        return
    explored.append(v)
    if v == 'end':
        paths.add(tuple(explored))
        return
    for e in adjacent[v]:
        if e.isupper():
            search(list(explored), e)
        elif e not in explored:
            search(list(explored), e)


with open('input.txt') as io:
    adjacent = {}  # type: dict[str, list[str]]
    for line in io.read().splitlines():
        v, w = line.split('-')
        adjacent.setdefault(v, []).append(w)
        adjacent.setdefault(w, []).append(v)
    search([], 'start')
    print(len(paths))

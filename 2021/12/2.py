from io import open
from collections import Counter

paths = set()


def search(explored: list, v: str):
    explored_counter = Counter(filter(str.islower, explored))
    if v.islower() and v in explored and explored_counter.most_common(1).pop()[1] == 2:
        return
    explored.append(v)
    if v == 'end':
        paths.add(tuple(explored))
        return
    for e in adjacent[v]:
        if e.isupper():
            search(list(explored), e)
        elif e == 'start':
            continue
        elif e not in explored or explored_counter.most_common(1).pop()[1] == 1:
            search(list(explored), e)


with open('input.txt') as io:
    adjacent = {}  # type: dict[str, list[str]]
    for line in io.read().splitlines():
        v, w = line.split('-')
        adjacent.setdefault(v, []).append(w)
        adjacent.setdefault(w, []).append(v)
    search([], 'start')
    print(len(paths))

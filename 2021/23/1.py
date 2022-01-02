from copy import deepcopy
from sys import maxsize
from typing import Dict, Iterator, List, Set, Tuple
import numpy as np


def update(graph, position, move):
    # type: (Dict[Tuple[int, int], str], Tuple[int, int], Tuple[int, int]) -> Dict[Tuple[int, int], str]
    next_graph = deepcopy(graph)
    next_graph[move] = next_graph[position]
    next_graph[position] = '.'
    return next_graph


def get_amphipods(graph):
    # type: (Dict[Tuple[int, int], str]) -> Iterator[Tuple[int, int]]
    for position, label in graph.items():
        if label != '.':
            if position in siderooms[label] and graph[siderooms[label][-1]] == label:
                pass
            else:
                yield position


def get_moves(graph, position):
    # type: (Dict[Tuple[int, int], str], Tuple[int, int]) -> List[Tuple[Tuple[int, int], int]]
    goal = get_goal(graph, graph[position])
    if goal is None:
        return []
    energy = energies[graph[position]]
    distance = {position: 0}  # type: Dict[Tuple[int, int], int]
    queue = [position]
    while queue:
        v = queue.pop()
        if v == goal:
            return [(v, distance[v])]
        for edge in edges[v]:
            if edge not in distance and graph[edge] == '.':
                distance[edge] = distance[v] + energy
                queue.append(edge)
    if position in hallways:
        return []
    distance.pop(position)
    return [(move, energy) for move, energy in distance.items() if move in hallways]


def get_goal(graph, label):
    # type: (Dict[Tuple[int, int], str], str) -> Tuple[int, int] | None
    if label == 'A':
        return (3, 3) if graph[3, 3] != 'A' else (3, 2) if graph[3, 2] != 'A' else None
    elif label == 'B':
        return (5, 3) if graph[5, 3] != 'B' else (5, 2) if graph[5, 2] != 'B' else None
    elif label == 'C':
        return (7, 3) if graph[7, 3] != 'C' else (7, 2) if graph[7, 2] != 'C' else None
    else:
        return (9, 3) if graph[9, 3] != 'D' else (9, 2) if graph[9, 2] != 'D' else None


def is_organized(graph):
    return graph[3, 2] == 'A' and graph[3, 3] == 'A' and graph[5, 2] == 'B' and graph[5, 3] == 'B' and graph[7, 2] == 'C' and graph[7, 3] == 'C' and graph[9, 2] == 'D' and graph[9, 3] == 'D'


def eliminate_branch(graph):
    count = 0
    for position in get_amphipods(graph):
        if position in hallways:
            count += 1
    return count > 2


def minimax(graph: Dict[Tuple[int, int], str], total_energy, depth):
    if is_organized(graph):
        global least_energy
        least_energy = min(total_energy, least_energy)
    else:
        if depth == 0 or eliminate_branch(graph):
            return maxsize
        for position in get_amphipods(graph):
            moves = get_moves(graph, position)
            for move, energy in moves:
                minimax(update(graph, position, move),
                        total_energy + energy, depth - 1)


def print_graph(graph: Dict[Tuple[int, int], str]):
    array = np.copy(grid)
    for (x, y), label in graph.items():
        array[y, x] = str(label)
    print()
    print(array)


with open('input.txt') as io:
    grid = [list(line) for line in io.read().splitlines()]
    graph: Dict[Tuple[int, int], str] = {}
    edges: Dict[Tuple[int, int], Set[Tuple[int, int]]] = {}
    siderooms: Dict[str, List[Tuple[int, int]]] = {'A': [(3, 2), (3, 3)],
                                                   'B': [(5, 2), (5, 3)],
                                                   'C': [(7, 2), (7, 3)],
                                                   'D': [(9, 2), (9, 3)]}
    energies = {'A': 1, 'B': 10, 'C': 100, 'D': 1000}
    types = {'.', 'A', 'B', 'C', 'D'}
    for y in range(len(grid)):
        for x in range(len(grid[y])):
            if grid[y][x] in types:
                graph[(x, y)] = grid[y][x]
                if (x - 1, y) in graph:
                    edges.setdefault((x, y), set()).add((x - 1, y))
                    edges.setdefault((x - 1, y), set()).add((x, y))
                if (x + 1, y) in graph:
                    edges.setdefault((x, y), set()).add((x + 1, y))
                    edges.setdefault((x + 1, y), set()).add((x, y))
                if (x, y - 1) in graph:
                    edges.setdefault((x, y), set()).add((x, y - 1))
                    edges.setdefault((x, y - 1), set()).add((x, y))
                if (x, y + 1) in graph:
                    edges.setdefault((x, y), set()).add((x, y + 1))
                    edges.setdefault((x, y + 1), set()).add((x, y))
    hallways = {position for position, label in graph.items()
                if label == '.' and len(edges[position]) != 3}
    least_energy = maxsize
    minimax(graph, 0, 50)
    print(least_energy)

from sys import maxsize
from typing import Dict, Iterator, List, Set, Tuple
import numpy as np


def update(graph, position, move):
    # type: (Dict[Tuple[int, int], str], Tuple[int, int], Tuple[int, int]) -> None
    graph[move] = graph[position]
    graph[position] = '.'


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
    for position in reversed(siderooms[label]):
        if graph[position] != label:
            return position
    return None


def is_organized(graph):
    for label, positions in siderooms.items():
        for position in positions:
            if graph[position] != label:
                return False
    return True


def minimax(graph: Dict[Tuple[int, int], str], total_energy, depth):
    if is_organized(graph):
        global least_energy
        least_energy = min(total_energy, least_energy)
    else:
        if depth == 0 or total_energy > least_energy:
            return
        for position in get_amphipods(graph):
            moves = get_moves(graph, position)
            for move, energy in moves:
                update(graph, position, move)
                minimax(graph, total_energy + energy, depth - 1)
                update(graph, move, position)


def print_graph(graph: Dict[Tuple[int, int], str]):
    array = np.copy(grid)
    for (x, y), label in graph.items():
        array[y, x] = str(label)
    print()
    print(array)


with open('input.txt') as io:
    grid = [list(line) for line in io.read().splitlines()]
    grid.insert(3, list('  #D#B#A#C#  '))
    grid.insert(3, list('  #D#C#B#A#  '))
    graph: Dict[Tuple[int, int], str] = {}
    edges: Dict[Tuple[int, int], Set[Tuple[int, int]]] = {}
    siderooms: Dict[str, List[Tuple[int, int]]] = {'A': [(3, 2), (3, 3), (3, 4), (3, 5)],
                                                   'B': [(5, 2), (5, 3), (5, 4), (5, 5)],
                                                   'C': [(7, 2), (7, 3), (7, 4), (7, 5)],
                                                   'D': [(9, 2), (9, 3), (9, 4), (9, 5)]}
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

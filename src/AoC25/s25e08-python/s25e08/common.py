"""Common utilities for this day's solution."""

from collections import Counter
from math import prod, sqrt


def parse_input(
    input_text: str,
) -> tuple[list[tuple[int, ...]], list[tuple[float, tuple[int, int]]]]:
    """Parse input and compute sorted distances between all point pairs."""
    data = [tuple(map(int, line.split(","))) for line in input_text.strip().split("\n")]

    distance_list = [
        (distance(data[a], data[b]), (a, b))
        for a in range(len(data))
        for b in range(a + 1, len(data))
    ]

    distances = sorted(distance_list)

    return data, distances


def solve_part1(input_text: str) -> int:
    """Solve part 1."""
    data, distances = parse_input(input_text)

    edge_count = 1000 if len(data) > 20 else 10

    # Build adjacency list using closest edges
    edges = [[] for _ in data]
    for _, (a, b) in distances[:edge_count]:
        edges[a].append(b)
        edges[b].append(a)

    # Color each connected component
    colors = [-1] * len(data)
    component_sizes = Counter()
    for node in range(len(data)):
        fill(node, edges, colors, node, component_sizes)

    # Return product of three largest components
    top3 = sorted(component_sizes.values(), reverse=True)[:3]

    return prod(top3)


def solve_part2(input_text: str) -> int:
    """Solve part 2."""
    data, distances = parse_input(input_text)

    colors = [-1] * len(data)
    colors[0] = 0
    component_sizes = Counter({0: 1})

    edges = [[] for _ in data]
    for _, (a, b) in distances:
        edges[a].append(b)
        edges[b].append(a)

        if colors[a] == 0 and colors[b] != 0:
            fill(b, edges, colors, 0, component_sizes)
        elif colors[b] == 0 and colors[a] != 0:
            fill(a, edges, colors, 0, component_sizes)

        if component_sizes[0] == len(data):
            return data[a][0] * data[b][0]

    return -1


def distance(p1: tuple[int, ...], p2: tuple[int, ...]) -> float:
    """Calculate Euclidean distance between two 3D points."""
    x1, y1, z1 = p1
    x2, y2, z2 = p2
    return sqrt((x2 - x1) ** 2 + (y2 - y1) ** 2 + (z2 - z1) ** 2)


def fill(
    node: int, edges: list[list[int]], colors: list[int], color: int, counter: Counter
) -> None:
    """Fill connected component starting from node using iterative DFS."""
    if colors[node] != -1:
        return

    stack = [node]
    while stack:
        current = stack.pop()
        if colors[current] != -1:
            continue

        colors[current] = color
        counter[color] += 1
        stack.extend(neighbor for neighbor in edges[current] if colors[neighbor] == -1)

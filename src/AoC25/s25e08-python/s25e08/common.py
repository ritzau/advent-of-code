"""Common utilities for this day's solution."""

from collections import Counter
from math import prod, sqrt


def parse_input(
    input_text: str,
) -> tuple[list[tuple[int, ...]], list[tuple[float, tuple[int, int]]]]:
    """Parse input and compute sorted distances between all point pairs."""
    import time

    t0 = time.perf_counter()
    data = [tuple(map(int, line.split(","))) for line in input_text.strip().split("\n")]
    t1 = time.perf_counter()
    print(f"    parse lines: {(t1 - t0) * 1000:.2f}ms")

    t2 = time.perf_counter()
    distance_list = [
        (distance(*data[a], *data[b]), (a, b))
        for a in range(len(data))
        for b in range(a + 1, len(data))
    ]
    t3 = time.perf_counter()
    print(f"    compute distances: {(t3 - t2) * 1000:.2f}ms ({len(distance_list)} pairs)")

    distances = sorted(distance_list)
    t4 = time.perf_counter()
    print(f"    sort distances: {(t4 - t3) * 1000:.2f}ms")

    return data, distances


def solve_part1(input_text: str) -> int:
    """Solve part 1."""
    import time

    t0 = time.perf_counter()
    data, distances = parse_input(input_text)
    t1 = time.perf_counter()
    print(f"  parse_input: {(t1 - t0) * 1000:.2f}ms")

    edge_count = 1000 if len(data) > 20 else 10

    # Build adjacency list using closest edges
    edges = [[] for _ in data]
    for _, (a, b) in distances[:edge_count]:
        edges[a].append(b)
        edges[b].append(a)
    t2 = time.perf_counter()
    print(f"  build edges: {(t2 - t1) * 1000:.2f}ms")

    # Color each connected component
    colors = [-1] * len(data)
    component_sizes = Counter()
    for node in range(len(data)):
        fill(node, edges, colors, node, component_sizes)
    t3 = time.perf_counter()
    print(f"  fill/color:  {(t3 - t2) * 1000:.2f}ms")

    # Return product of three largest components
    top3 = sorted(component_sizes.values(), reverse=True)[:3]
    t4 = time.perf_counter()
    print(f"  compute top3: {(t4 - t3) * 1000:.2f}ms")

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


def distance(*coords: int) -> float:
    """Calculate Euclidean distance between two 3D points."""
    x1, y1, z1, x2, y2, z2 = coords
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

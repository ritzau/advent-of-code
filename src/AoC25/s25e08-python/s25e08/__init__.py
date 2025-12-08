"""Advent of Code solution."""

from math import prod, sqrt
import sys
from .common import parse_input

__all__ = ["parse_input", "solve_part1", "solve_part2"]

def solve_part1(input_text: str) -> int:
    """Solve part 1."""

    lines = parse_input(input_text)
    # print(lines)
    data = [tuple(map(int, line.split(","))) for line in lines]

    l = sorted([
        (distance(x1, y1, z1, x2, y2, z2), (a, b)) 
            for a, (x1, y1, z1) in enumerate(data) 
            for b, (x2, y2, z2) in enumerate(data)
            if a < b
    ])
   
    edges = [[] for _ in data]
    for _, (a, b) in l[:1000]:
        # print(f"{a} <-> {b}")
        edges[a].append(b)
        edges[b].append(a)

    # print("\n".join(str(row) for row in l))
    # print("\n".join(str(edge) for edge in edges))

    colors = [-1] * len(data)
    for node, _ in enumerate(edges):
        fill(node, edges, colors, node)

    # print(colors)

    histogram = [0] * len(data)
    for color in colors:
        histogram[color] += 1

    # print(histogram)

    top3 = sorted([count for count in histogram if count > 0])[-3:]
    # print(f"Top 3: {top3}")
    product = prod(top3)
    # print(f"Product: {product}")

    return product

node_count = 0

def solve_part2(input_text: str) -> int:
    """Solve part 2."""
    global node_count
    node_count = 1

    lines = parse_input(input_text)
    data = [tuple(map(int, line.split(","))) for line in lines]
    l = sorted([
        (distance(x1, y1, z1, x2, y2, z2), (a, b)) 
            for a, (x1, y1, z1) in enumerate(data) 
            for b, (x2, y2, z2) in enumerate(data)
            if a < b
    ])

    colors = [-1] * len(data)
    colors[0] = 0

    edges = [[] for _ in data]
    for _, (a, b) in l:
        edges[a].append(b)
        edges[b].append(a)

        if colors[a] == 0 and colors[b] != 0:
            fill(b, edges, colors, 0)
        elif colors[b] == 0 and colors[a] != 0:
            fill(a, edges, colors, 0)

        if node_count == len(data):
            return data[a][0] * data[b][0]

    return -1

def distance(x1: int, y1: int, z1: int, x2: int, y2: int, z2: int) -> int:
    return sqrt((x2 - x1) ** 2 + (y2 - y1) ** 2 + (z2 - z1) ** 2)

def fill(node: int, edges: list[int], colors: list[int], color: int) -> None:
    global node_count
    
    if colors[node] != -1:
        return
    
    stack = [node]
    while stack:
        current = stack.pop()
        if colors[current] != -1:
            continue
        
        colors[current] = color
        node_count += 1
        
        for neighbor in edges[current]:
            if colors[neighbor] == -1:
                stack.append(neighbor)
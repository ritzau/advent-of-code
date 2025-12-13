use std::collections::HashMap;

type Graph = HashMap<String, Vec<String>>;

pub fn parse_input(input: &str) -> Graph {
    let mut graph = Graph::new();

    for line in input.trim().lines() {
        let parts: Vec<&str> = line.split(": ").collect();
        let name = parts[0].to_string();

        if parts.len() > 1 {
            let children: Vec<String> = parts[1].split(' ').map(String::from).collect();
            graph.insert(name, children);
        }
    }

    graph
}

fn count_paths(node: &str, graph: &Graph) -> i64 {
    if node == "out" {
        return 1;
    }

    graph
        .get(node)
        .map(|children| children.iter().map(|child| count_paths(child, graph)).sum())
        .unwrap_or(0)
}

pub fn solve_part1(input: &str) -> i64 {
    let graph = parse_input(input);
    count_paths("you", &graph)
}

pub fn solve_part2(input: &str) -> i64 {
    let _graph = parse_input(input);
    // TODO: Implement solution
    0
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE_INPUT: &str = "\
aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out";

    #[test]
    fn part1_sample_1() {
        assert_eq!(solve_part1(SAMPLE_INPUT), 5);
    }

    #[test]
    fn part2_sample_1() {
        assert_eq!(solve_part2(SAMPLE_INPUT), 0);
    }
}

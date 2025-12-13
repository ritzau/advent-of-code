use std::collections::HashMap;

type Graph<'a> = HashMap<&'a str, Vec<&'a str>>;

pub fn parse_input(input: &str) -> Graph<'_> {
    let mut graph = Graph::new();

    for line in input.trim().lines() {
        let parts: Vec<&str> = line.split(": ").collect();
        let name = parts[0];

        if parts.len() > 1 {
            let children: Vec<&str> = parts[1].split(' ').collect();
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

fn count_paths_with_required_helper<'a>(
    node: &'a str,
    graph: &Graph<'a>,
    seen_fft: bool,
    seen_dac: bool,
    memo: &mut HashMap<(&'a str, bool, bool), i64>,
) -> i64 {
    if node == "out" {
        return if seen_fft && seen_dac { 1 } else { 0 };
    }

    let memo_key = (node, seen_fft, seen_dac);
    if let Some(&cached) = memo.get(&memo_key) {
        return cached;
    }

    let new_seen_fft = seen_fft || node == "fft";
    let new_seen_dac = seen_dac || node == "dac";

    let result = graph
        .get(node)
        .map(|children| {
            children
                .iter()
                .map(|child| {
                    count_paths_with_required_helper(child, graph, new_seen_fft, new_seen_dac, memo)
                })
                .sum()
        })
        .unwrap_or(0);

    memo.insert(memo_key, result);
    result
}

fn count_paths_with_required<'a>(node: &'a str, graph: &Graph<'a>) -> i64 {
    let mut memo = HashMap::new();
    count_paths_with_required_helper(node, graph, false, false, &mut memo)
}

pub fn solve_part1(input: &str) -> i64 {
    let graph = parse_input(input);
    count_paths("you", &graph)
}

pub fn solve_part2(input: &str) -> i64 {
    let graph = parse_input(input);
    count_paths_with_required("svr", &graph)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn part1_sample_1() {
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

        assert_eq!(solve_part1(SAMPLE_INPUT), 5);
    }

    #[test]
    fn part2_sample_1() {
        const SAMPLE_INPUT: &str = "\
svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out";

        assert_eq!(solve_part2(SAMPLE_INPUT), 2);
    }
}

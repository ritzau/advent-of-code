#!/usr/bin/env python3
"""
Statically list `packages` and `apps` attribute names from `flake.nix` files
without evaluating Nix. This is a heuristic parser (not a full Nix parser)
but works for the common layout used in this repository.

Usage: python3 scripts/list-flake-outputs.py
"""
import pathlib
import re


def extract_attrs(block_text):
    """Return top-level attribute names in a `{ ... }` block.
    We look for lines like `name =`, `name = {`, or `name = (` or `name = pkgs...`.
    """
    names = []
    for line in block_text.splitlines():
        # remove comments
        line = re.sub(r"#.*", "", line).strip()
        if not line:
            continue
        m = re.match(r"^([A-Za-z0-9_+-]+)\s*=", line)
        if m:
            names.append(m.group(1))
    return names


def parse_flake(path: pathlib.Path):
    text = path.read_text()
    results = {}

    # find packages = { ... } and apps = { ... } blocks by naive brace matching
    for key in ("packages", "apps", "checks", "devShells"):
        idx = text.find(key + " = {")
        if idx == -1:
            # also allow `key = {` with newlines: search for `key` followed by `=` then `{`
            m = re.search(r"\b" + re.escape(key) + r"\b\s*=\s*{", text)
            if m:
                idx = m.start()
        if idx == -1:
            continue
        # find block start (first '{' after '=')
        start = text.find('{', idx)
        if start == -1:
            continue
        depth = 0
        end = start
        for i, ch in enumerate(text[start:], start):
            if ch == '{':
                depth += 1
            elif ch == '}':
                depth -= 1
                if depth == 0:
                    end = i
                    break
        block = text[start+1:end]
        results[key] = extract_attrs(block)

    return results


def main():
    repo = pathlib.Path(__file__).resolve().parent.parent
    flakes = list(repo.rglob('flake.nix'))
    flakes = [p for p in flakes if '.direnv' not in str(p) and '/.direnv/' not in str(p)]

    out = []
    for f in sorted(flakes):
        rel = f.relative_to(repo)
        parsed = parse_flake(f)
        if parsed:
            out.append((str(rel.parent), parsed))

    if not out:
        print("No flake.nix files with packages/apps found.")
        return

    for loc, parsed in out:
        print(f"{loc}:")
        for k in ('packages', 'apps', 'checks', 'devShells'):
            vals = parsed.get(k)
            if vals:
                print(f"  {k}:")
                for name in vals:
                    print(f"    - {name}")
        print()


if __name__ == '__main__':
    main()

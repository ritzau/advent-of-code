# Advent of Code - Just Commands

# List available commands
default:
    @just --list

# Download input for a specific day
download YEAR DAY:
    @python3 scripts/download-input.py {{YEAR}} {{DAY}}

# Run a specific day's solution (both parts)
run YEAR DAY:
    @echo "🎄 Advent of Code {{YEAR}} - Day {{DAY}}"
    @echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    @just _ensure-input {{YEAR}} {{DAY}}
    @just _run-parts {{YEAR}} {{DAY}} "inputs/{{YEAR}}/day{{DAY}}.txt"

# Run all days for a year
run-all YEAR:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "🎄 Advent of Code {{YEAR}} - All Days"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    for day_dir in src/AoC{{YEAR}}/day*/; do
        if [ -d "$day_dir" ]; then
            day=$(basename "$day_dir" | sed 's/day0*//')
            echo ""
            just run {{YEAR}} $day || true
        fi
    done
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# Test with sample input
test YEAR DAY:
    @echo "🧪 Testing AoC {{YEAR}} Day {{DAY}} with sample"
    @echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    @just _run-parts {{YEAR}} {{DAY}} "src/AoC{{YEAR}}/day{{DAY}}/sample.txt"

# Create a new day from template
new YEAR DAY LANGUAGE:
    #!/usr/bin/env bash
    set -euo pipefail
    day_padded=$(printf "%02d" {{DAY}})
    dest="src/AoC{{YEAR}}/day$day_padded"

    if [ -d "$dest" ]; then
        echo "Error: $dest already exists"
        exit 1
    fi

    template="templates/{{LANGUAGE}}"
    if [ ! -d "$template" ]; then
        echo "Error: Template for {{LANGUAGE}} not found"
        echo "Available templates:"
        ls -1 templates/
        exit 1
    fi

    echo "Creating day $day_padded for {{YEAR}} using {{LANGUAGE}} template..."
    cp -r "$template" "$dest"

    # Create empty sample.txt
    touch "$dest/sample.txt"

    echo "✓ Created $dest"
    echo "  Edit sample.txt and run: just test {{YEAR}} {{DAY}}"
    echo "  Download input with: just download {{YEAR}} {{DAY}}"

# Setup a new day (download input + create from template)
setup YEAR DAY LANGUAGE:
    @just new {{YEAR}} {{DAY}} {{LANGUAGE}}
    @just download {{YEAR}} {{DAY}}

# Clean all cached inputs
clean-inputs:
    rm -rf inputs/
    @echo "✓ Cleaned all cached inputs"

# Internal: Ensure input is downloaded
_ensure-input YEAR DAY:
    #!/usr/bin/env bash
    day_padded=$(printf "%02d" {{DAY}})
    input_file="inputs/{{YEAR}}/day$day_padded.txt"
    if [ ! -f "$input_file" ]; then
        python3 scripts/download-input.py {{YEAR}} {{DAY}}
    fi

# Internal: Run both parts and format output
_run-parts YEAR DAY INPUT:
    #!/usr/bin/env bash
    set -euo pipefail
    day_padded=$(printf "%02d" {{DAY}})
    day_dir="src/AoC{{YEAR}}/day$day_padded"

    if [ ! -d "$day_dir" ]; then
        echo "Error: $day_dir does not exist"
        exit 1
    fi

    cd "$day_dir"

    # Detect how to run (check for shell.nix, various file types)
    if [ -f "shell.nix" ]; then
        run_cmd="nix-shell --run"
    else
        run_cmd="bash -c"
    fi

    # Detect language and run appropriately
    input_path="../../$INPUT"

    # Part 1
    if [ -f "part1.py" ]; then
        cmd="cat $input_path | python3 part1.py"
    elif [ -f "part1.rs" ] || [ -f "Cargo.toml" ]; then
        cmd="cat $input_path | cargo run --quiet --release --bin part1"
    elif [ -f "part1.go" ]; then
        cmd="cat $input_path | go run part1.go"
    elif [ -f "part1.kt" ]; then
        cmd="cat $input_path | kotlinc -script part1.kts"
    elif [ -f "part1.nim" ]; then
        cmd="cat $input_path | nim r -d:release --hints:off part1.nim"
    elif [ -f "part1.zig" ]; then
        cmd="cat $input_path | zig run part1.zig"
    else
        echo "Error: Cannot determine how to run part1"
        exit 1
    fi

    echo -n "Part 1: "
    start=$(date +%s%N)
    result1=$($run_cmd "$cmd" 2>&1) || { echo "❌ Failed"; exit 1; }
    end=$(date +%s%N)
    duration1=$(echo "scale=1; ($end - $start) / 1000000" | bc)
    echo "$result1 (${duration1}ms)"

    # Part 2
    if [ -f "part2.py" ]; then
        cmd="cat $input_path | python3 part2.py"
    elif [ -f "part2.rs" ] || [ -f "Cargo.toml" ]; then
        cmd="cat $input_path | cargo run --quiet --release --bin part2"
    elif [ -f "part2.go" ]; then
        cmd="cat $input_path | go run part2.go"
    elif [ -f "part2.kt" ]; then
        cmd="cat $input_path | kotlinc -script part2.kts"
    elif [ -f "part2.nim" ]; then
        cmd="cat $input_path | nim r -d:release --hints:off part2.nim"
    elif [ -f "part2.zig" ]; then
        cmd="cat $input_path | zig run part2.zig"
    else
        echo "Part 2: Not yet implemented"
        exit 0
    fi

    echo -n "Part 2: "
    start=$(date +%s%N)
    result2=$($run_cmd "$cmd" 2>&1) || { echo "❌ Failed"; exit 1; }
    end=$(date +%s%N)
    duration2=$(echo "scale=1; ($end - $start) / 1000000" | bc)
    echo "$result2 (${duration2}ms)"

    total=$(echo "scale=1; $duration1 + $duration2" | bc)
    echo "Total: ${total}ms"

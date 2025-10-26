# Advent of Code - Just Commands

# List available commands
default:
    @just --list

# Download input for a specific day
download YEAR DAY:
    @python3 scripts/download-input.py {{YEAR}} {{DAY}}

# Run a specific day's solution (both parts)
run YEAR DAY:
    #!/usr/bin/env bash
    day_padded=$(printf "%02d" {{DAY}})
    echo "🎄 Advent of Code {{YEAR}} - Day {{DAY}}"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    just _ensure-input {{YEAR}} {{DAY}}
    just _run-parts {{YEAR}} {{DAY}} "inputs/{{YEAR}}/day${day_padded}.txt"

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
    #!/usr/bin/env bash
    day_padded=$(printf "%02d" {{DAY}})
    echo "🧪 Testing AoC {{YEAR}} Day {{DAY}} with sample"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    just _run-parts {{YEAR}} {{DAY}} "src/AoC{{YEAR}}/day${day_padded}/sample.txt"

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

    # Check for run.sh script
    if [ ! -f "run.sh" ]; then
        echo "Error: run.sh not found in $day_dir"
        exit 1
    fi

    chmod +x run.sh
    input_path="../../$INPUT"

    # Detect how to run (with or without Nix flake)
    if [ -f "flake.nix" ]; then
        runner="nix develop --command bash -c"
    else
        runner="bash -c"
    fi

    # Part 1
    echo -n "Part 1: "
    start=$(date +%s%N)
    result1=$($runner "cat $input_path | ./run.sh part1" 2>&1) || { echo "❌ Failed"; exit 1; }
    end=$(date +%s%N)
    duration1=$(echo "scale=1; ($end - $start) / 1000000" | bc)
    echo "$result1 (${duration1}ms)"

    # Part 2
    echo -n "Part 2: "
    start=$(date +%s%N)
    result2=$($runner "cat $input_path | ./run.sh part2" 2>&1)
    if [ $? -ne 0 ]; then
        echo "Not yet implemented"
    else
        end=$(date +%s%N)
        duration2=$(echo "scale=1; ($end - $start) / 1000000" | bc)
        echo "$result2 (${duration2}ms)"

        total=$(echo "scale=1; $duration1 + $duration2" | bc)
        echo "Total: ${total}ms"
    fi

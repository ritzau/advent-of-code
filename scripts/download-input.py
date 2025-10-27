#!/usr/bin/env python3
"""
Download Advent of Code input files.
Usage: download-input.py YEAR DAY
"""

import sys
import os
from pathlib import Path
import urllib.request
import urllib.error

def get_session_cookie():
    """Read session cookie from .aoc-session file."""
    session_file = Path(".aoc-session")

    if not session_file.exists():
        print("Error: .aoc-session file not found", file=sys.stderr)
        print("Create a .aoc-session file with your session cookie from adventofcode.com", file=sys.stderr)
        print("You can find it in your browser cookies after logging in.", file=sys.stderr)
        sys.exit(1)

    session = session_file.read_text().strip()
    if not session:
        print("Error: .aoc-session file is empty", file=sys.stderr)
        sys.exit(1)

    return session

def download_input(year, day):
    """Download input for given year and day."""
    session = get_session_cookie()

    # Create cache directory
    cache_dir = Path("inputs") / str(year)
    cache_dir.mkdir(parents=True, exist_ok=True)

    # Cache file path
    cache_file = cache_dir / f"day{day:02d}.txt"

    # Check if already cached
    if cache_file.exists():
        print(f"Using cached input: {cache_file}")
        return cache_file

    # Download from adventofcode.com
    url = f"https://adventofcode.com/{year}/day/{day}/input"
    print(f"Downloading from {url}...")

    try:
        req = urllib.request.Request(url)
        req.add_header('Cookie', f'session={session}')
        req.add_header('User-Agent', 'github.com/ritzau/advent-of-code by contact@example.com')

        with urllib.request.urlopen(req) as response:
            content = response.read().decode('utf-8')

            # Save to cache
            cache_file.write_text(content)
            print(f"Downloaded and cached: {cache_file}")
            return cache_file

    except urllib.error.HTTPError as e:
        if e.code == 404:
            print(f"Error: Input not available yet for {year} day {day}", file=sys.stderr)
        elif e.code == 400:
            print(f"Error: Invalid session cookie", file=sys.stderr)
        else:
            print(f"Error: HTTP {e.code} - {e.reason}", file=sys.stderr)
        sys.exit(1)
    except urllib.error.URLError as e:
        print(f"Error: Failed to connect - {e.reason}", file=sys.stderr)
        sys.exit(1)

def main():
    if len(sys.argv) != 3:
        print("Usage: download-input.py YEAR DAY", file=sys.stderr)
        sys.exit(1)

    try:
        year = int(sys.argv[1])
        day = int(sys.argv[2])
    except ValueError:
        print("Error: YEAR and DAY must be integers", file=sys.stderr)
        sys.exit(1)

    if not (2015 <= year <= 2030):
        print(f"Error: Invalid year {year}", file=sys.stderr)
        sys.exit(1)

    if not (1 <= day <= 25):
        print(f"Error: Invalid day {day}", file=sys.stderr)
        sys.exit(1)

    download_input(year, day)

if __name__ == "__main__":
    main()

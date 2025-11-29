# Helper Scripts

## download-input.py

Downloads puzzle inputs from Advent of Code using your session cookie.

```bash
python3 scripts/download-input.py YEAR DAY
```

**Requirements:**
- `.aoc-session` file with your Advent of Code session cookie

**Output:**
- Downloads input to `inputs/YEAR/dayDD.txt`
- Caches inputs locally (gitignored)

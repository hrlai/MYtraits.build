#!/usr/bin/env python3
"""
Block `gh pr create` when the branch is behind origin/main.

Claude Code PreToolUse hook for the Bash tool. The tool call arrives as JSON
on stdin. Commands without `gh pr create` pass at once. Otherwise the hook
runs `git fetch origin` and counts the commits in origin/main that HEAD lacks.
If there are any, it exits 2 and tells the agent to merge origin/main first.

A failed fetch (offline, no remote) only warns: network errors never block.
"""
import json
import subprocess
import sys


def git(*args):
    return subprocess.run(
        ["git", *args], capture_output=True, text=True, timeout=60
    )


def main():
    try:
        payload = json.load(sys.stdin)
    except ValueError:
        return 0
    command = (payload.get("tool_input") or {}).get("command") or ""
    if "gh pr create" not in command:
        return 0

    try:
        fetch = git("fetch", "origin", "--quiet")
    except (OSError, subprocess.TimeoutExpired) as e:
        print(f"check_main_up_to_date: git fetch failed ({e}); "
              f"not checking origin/main.", file=sys.stderr)
        return 0
    if fetch.returncode != 0:
        print(f"check_main_up_to_date: git fetch origin failed; not checking "
              f"origin/main.\n{fetch.stderr.strip()}", file=sys.stderr)
        return 0

    count = git("rev-list", "--count", "HEAD..origin/main")
    if count.returncode != 0:
        print(f"check_main_up_to_date: could not compare with origin/main; "
              f"not checking.\n{count.stderr.strip()}", file=sys.stderr)
        return 0

    behind = int(count.stdout.strip() or 0)
    if behind > 0:
        print(
            f"This branch is {behind} commit(s) behind origin/main. Run "
            f"`git merge origin/main`, rerun the checks, and say in the PR "
            f"that main was merged. Then open the PR.",
            file=sys.stderr,
        )
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())

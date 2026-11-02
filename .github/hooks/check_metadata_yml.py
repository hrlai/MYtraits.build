#!/usr/bin/env python3
"""
Check data/<id>/metadata.yml files for mistakes that build without error but
give wrong data. Adapted from the austraits.build Claude Code hook.

Two ways to run:
  - pre-commit (and by hand): file paths as arguments. Prints problems and
    exits 1 if any are found.
  - Claude Code PostToolUse hook: no arguments, the tool call arrives as JSON
    on stdin. Problems go back to the agent as additional context.

Checks:
  A. A value parsed as a YAML boolean instead of a string. Bare y/n/yes/no
     (any case) parse as booleans.
  B. `custom_R_code:` not directly followed by `collection_date:`.
     traits.build's read_metadata() reads custom_R_code from the raw lines up
     to the next `  collection_date:` line, so any field between them is
     swallowed into the R code.
  C. Reminder only: every trait is entity_type: species while locations:
     holds real sites. dataset_process() sets location_id to NA for species
     rows, so the sites end up linked to no trait value.
"""
import json
import re
import sys

import yaml

BOOL_WHITELIST = {"dataset.data_is_long_format"}
METADATA_PATH = re.compile(r"(^|/)data/[^/]+/metadata\.yml$")


class RYamlLoader(yaml.SafeLoader):
    """SafeLoader that also reads bare y/n as booleans, as R's yaml does."""


RYamlLoader.add_implicit_resolver(
    "tag:yaml.org,2002:bool", re.compile(r"^(?:y|Y|n|N)$"), list("yYnN")
)
RYamlLoader.add_constructor(
    "tag:yaml.org,2002:bool",
    lambda loader, node: loader.construct_scalar(node).lower() in
    ("y", "yes", "true", "on"),
)


def find_booleans(node, path, out):
    if isinstance(node, dict):
        for k, v in node.items():
            find_booleans(v, path + [str(k)], out)
    elif isinstance(node, list):
        for i, v in enumerate(node):
            find_booleans(v, path + [str(i)], out)
    elif isinstance(node, bool):
        p = ".".join(path)
        if p not in BOOL_WHITELIST:
            out.append((p, node))


def check_custom_r_code_adjacency(lines):
    for i, line in enumerate(lines):
        if re.match(r"^  custom_R_code:", line):
            for j in range(i + 1, len(lines)):
                m = re.match(r"^  ([A-Za-z0-9_]+):", lines[j])
                if m:
                    if m.group(1) != "collection_date":
                        return [
                            f"`custom_R_code:` (line {i + 1}) is followed by "
                            f"`{m.group(1)}:` (line {j + 1}), not "
                            f"`collection_date:`. read_metadata() swallows "
                            f"every line up to `collection_date:` into the R "
                            f"code. Move `collection_date:` directly after "
                            f"`custom_R_code:`."
                        ]
                    return []
            return []
    return []


def check_file(path):
    """Return (notes, reminders) for one metadata.yml."""
    with open(path, encoding="utf-8") as f:
        text = f.read()
    notes = []
    reminders = []

    try:
        data = yaml.load(text, Loader=RYamlLoader)
    except yaml.YAMLError as e:
        notes.append(f"File does not parse as YAML: {e}")
        data = None

    if data is not None:
        hits = []
        find_booleans(data, [], hits)
        for p, v in hits:
            notes.append(
                f"`{p}` parsed as the YAML boolean `{v}`, not a string. "
                f"Bare y/n/yes/no parse as booleans. Quote it (`'n'`) if "
                f"the text was meant."
            )

    notes.extend(check_custom_r_code_adjacency(text.splitlines()))

    if isinstance(data, dict):
        locs = data.get("locations")
        traits = data.get("traits")
        if isinstance(locs, dict) and locs and isinstance(traits, list) and traits:
            types = {t.get("entity_type") for t in traits if isinstance(t, dict)}
            if types == {"species"}:
                reminders.append(
                    "Every trait is `entity_type: species` while `locations:` "
                    "holds sites. dataset_process() sets location_id to NA for "
                    "species rows, so no trait value links to these sites. "
                    "Fine if the sites are kept for provenance only."
                )
    return notes, reminders


def report(path, notes, reminders):
    parts = []
    if notes:
        parts.append(
            f"metadata.yml check, likely problems in {path}:\n"
            + "\n".join(f"- {n}" for n in notes)
        )
    if reminders:
        parts.append(
            f"metadata.yml check, for your awareness in {path}:\n"
            + "\n".join(f"- {n}" for n in reminders)
        )
    return "\n\n".join(parts)


def main_files(paths):
    failed = False
    for path in paths:
        if not METADATA_PATH.search(path.replace("\\", "/")):
            continue
        notes, reminders = check_file(path)
        msg = report(path, notes, reminders)
        if msg:
            print(msg)
        failed = failed or bool(notes)
    return 1 if failed else 0


def main_hook():
    try:
        payload = json.load(sys.stdin)
    except ValueError:
        return 0
    path = (payload.get("tool_input") or {}).get("file_path") or ""
    if not METADATA_PATH.search(path.replace("\\", "/")):
        return 0
    try:
        notes, reminders = check_file(path)
    except OSError:
        return 0
    msg = report(path, notes, reminders)
    if msg:
        print(json.dumps({
            "hookSpecificOutput": {
                "hookEventName": "PostToolUse",
                "additionalContext": msg,
            }
        }))
    return 0


if __name__ == "__main__":
    sys.exit(main_files(sys.argv[1:]) if len(sys.argv) > 1 else main_hook())

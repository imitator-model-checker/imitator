import itertools
import re
import sys
from typing import List, Dict, Set, Tuple
from pathlib import Path


# def parse_imi_file(filepath: str) -> Tuple[List[str], Dict[str, List[Tuple[str, str]]], str, Set[str], List[str]]:
#     with open(filepath, 'r') as file:
#         content = file.read()

#     # Extract all locations
#     location_blocks = re.findall(r"loc\s+(\w+):.*?goto.*?;", content, re.DOTALL)
#     locations = sorted(set(location_blocks))

#     # Extract transitions from each location
#     transitions = {}
#     for loc in locations:
#         pattern = rf"(accepting\s+)?loc\s+{loc}:.*?(?=(\n\s*(loc|accepting loc)|\Z))"
#         match = re.search(pattern, content, re.DOTALL)
#         if match:
#             block = match.group()
#             trans = re.findall(r"sync\s+(\w+).*?goto\s+(\w+)", block)
#             transitions[loc] = trans

#     # Extract initial location
#     init_match = re.search(r"loc\[barman\]\s*=\s*(\w+)", content)
#     initial_location = init_match.group(1) if init_match else None

#     # Extract accepting locations
#     accepting = set(re.findall(r"accepting loc\s+(\w+):", content))

#     # Extract sync labels
#     labels_match = re.search(r"synclabs:\s*([^;]+);", content)
#     labels = labels_match.group(1).split(",") if labels_match else []

#     return locations, transitions, initial_location, accepting, labels

def parse_imi_file(filepath: str) -> Tuple[List[str], Dict[str, List[Tuple[str, str]]], str, Set[str], List[str], str]:
    with open(filepath, 'r') as file:
        content = file.read()

    # Extract automaton name
    automaton_match = re.search(r"automaton\s+(\w+)", content)
    automaton_name = automaton_match.group(1) if automaton_match else "UNKNOWN"

    # (le reste ne change pas)
    location_blocks = re.findall(r"loc\s+(\w+):.*?goto.*?;", content, re.DOTALL)
    locations = sorted(set(location_blocks))

    transitions = {}
    for loc in locations:
        pattern = rf"(accepting\s+)?loc\s+{loc}:.*?(?=(\n\s*(loc|accepting loc)|\Z))"
        match = re.search(pattern, content, re.DOTALL)
        if match:
            block = match.group()
            trans = re.findall(r"sync\s+(\w+).*?goto\s+(\w+)", block)
            transitions[loc] = trans

    init_match = re.search(r"loc\[(\w+)\]\s*=\s*(\w+)", content)
    init_automaton = init_match.group(1) if init_match else automaton_name
    initial_location = init_match.group(2) if init_match else None

    accepting = set(re.findall(r"accepting loc\s+(\w+):", content))
    labels_match = re.search(r"synclabs:\s*([^;]+);", content)
    labels = labels_match.group(1).split(",") if labels_match else []

    return locations, transitions, initial_location, accepting, labels, init_automaton



def propagate(transitions: Dict[str, List[Tuple[str, str]]],
              source_set: Set[str],
              accepting_states: Set[str]) -> Dict[str, Set[str]]:
    label_to_targets_full = {}

    for src in source_set:
        for label, tgt in transitions.get(src, []):
            label_to_targets_full.setdefault(label, set()).add(tgt)

    label_to_filtered = {}
    for label, targets in label_to_targets_full.items():
        non_accepting = {t for t in targets if t not in accepting_states}
        if non_accepting:
            label_to_filtered[label] = non_accepting
        elif len(targets) == 1:
            # Only accepting state in the target, and it's alone → we keep it
            label_to_filtered[label] = targets

    return label_to_filtered


def format_location_name(loc_set: Set[str]) -> str:
    return "_".join(sorted(loc_set))


def translate_model(filepath: str, output_path: str):
    locations, transitions, initial_location, accepting_states, labels, automaton_name = parse_imi_file(filepath)

    worklist = []
    visited = set()
    reachable = {}

    init_infoset = set()
    for loc in locations:
        if loc not in accepting_states:
            init_infoset.add(loc)

    worklist.append(init_infoset)

    while worklist:
        current = worklist.pop()
        name = format_location_name(current)
        if name in visited:
            continue
        visited.add(name)

        img = propagate(transitions, current, accepting_states)
        reachable[name] = {}
        for label, tgt_set in img.items():
            tgt_name = format_location_name(tgt_set)
            if tgt_name and tgt_name not in visited:
                worklist.append(tgt_set)
            reachable[name][label] = tgt_name

    with open(output_path, 'w') as f:
        f.write("(************************************************************)\n")
        f.write(f"  automaton {automaton_name}\n")
        f.write("(************************************************************)\n")
        f.write("synclabs: {};\n\n".format(",".join(labels)))

        for state, label_targets in reachable.items():
            if not state in accepting_states:
                f.write(f"loc {state}: invariant True\n")
                for label, target in label_targets.items():
                    if target:
                        f.write(f"\twhen True sync {label} do {{}} goto {target};\n")
                f.write("\n")

        for acc in accepting_states:
            f.write(f"accepting loc {acc}: invariant True\t\n\n")

        f.write("end (* machine *)\n\n")
        f.write("(************************************************************)\n")
        f.write("(* Initial state *)\n")
        f.write("(************************************************************)\n\n")
        f.write("init :=\n")
        f.write("\t(*------------------------------------------------------------*)\n")
        f.write("\t(* Initial location *)\n")
        f.write("\t(*------------------------------------------------------------*)\n")
        f.write(f"\t& loc[{automaton_name}] = {format_location_name(init_infoset)}\n;\n")
        f.write("(************************************************************)\n")
        f.write("(* The end *)\n")
        f.write("(************************************************************)\n")
        f.write("end\n")


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python fichier.py model.imi")
        sys.exit(1)

    input_path = Path(sys.argv[1])
    if not input_path.exists() or not input_path.suffix == ".imi":
        print("Error: file does not exist or is not a .imi file.")
        sys.exit(1)

    output_name = f"info_less_{input_path.stem}.imi"
    translate_model(str(input_path), output_name)
    print(f"Model without information saved to {output_name}")

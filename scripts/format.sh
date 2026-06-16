#!/usr/bin/env bash
#
# ************************************************************
#                       IMITATOR
#
# Script description: format OCaml source files with ocamlformat.
#
# By default only formats files you have changed (working tree +
# new untracked files), to avoid rewriting the whole codebase at
# once. Use --all to format everything.
#
# Configuration is read from the repo's .ocamlformat file, and
# .ocamlformat-ignore is honoured automatically.
# ************************************************************

set -euo pipefail

MODE="modified"   # modified | staged | base | all
CHECK=0           # 0 = rewrite in place, 1 = check only (no changes)
BASE="origin/develop"

usage() {
  cat <<'EOF'
Usage: scripts/format.sh [options]

Format OCaml (.ml/.mli) source files with ocamlformat.

Scope (choose one; default: --modified):
  -m, --modified     Files changed in the working tree, plus new untracked
                     files (default).
  -s, --staged       Only files staged for the next commit.
  -b, --base <ref>   Files changed relative to a base ref (default:
                     origin/develop). Useful before opening a pull request.
  -a, --all          Every tracked .ml/.mli file in the repository.

Mode:
  -c, --check        Do not modify files; exit non-zero if any file is not
                     already formatted. Intended for CI and pre-commit hooks.

  -h, --help         Show this help.

Examples:
  scripts/format.sh                 # format files you changed
  scripts/format.sh --staged        # format what you are about to commit
  scripts/format.sh --base main     # format everything changed vs main
  scripts/format.sh --all --check   # CI gate over the whole tree
EOF
}

# --- parse arguments --------------------------------------------------------
while [ $# -gt 0 ]; do
  case "$1" in
    -m|--modified) MODE="modified" ;;
    -s|--staged)   MODE="staged" ;;
    -a|--all)      MODE="all" ;;
    -b|--base)
      MODE="base"
      shift
      [ $# -gt 0 ] || { echo "ERROR: --base requires a ref argument." >&2; exit 2; }
      BASE="$1"
      ;;
    -c|--check)    CHECK=1 ;;
    -h|--help)     usage; exit 0 ;;
    *) echo "ERROR: unknown option '$1'. Use --help." >&2; exit 2 ;;
  esac
  shift
done

# --- preconditions ----------------------------------------------------------
if ! command -v git >/dev/null 2>&1; then
  echo "ERROR: git is required but was not found." >&2
  exit 1
fi

ROOT="$(git rev-parse --show-toplevel 2>/dev/null)" || {
  echo "ERROR: not inside a git repository." >&2
  exit 1
}
cd "$ROOT"

if ! command -v ocamlformat >/dev/null 2>&1; then
  echo "ERROR: ocamlformat is not installed." >&2
  echo "       Install it with:  opam install ocamlformat" >&2
  echo "       (match the version pinned in .ocamlformat)." >&2
  exit 1
fi

# --- collect target files (NUL-delimited, handles spaces) -------------------
collect() {
  case "$MODE" in
    modified)
      git diff -z --name-only --diff-filter=ACMR HEAD -- '*.ml' '*.mli'
      git ls-files -z --others --exclude-standard -- '*.ml' '*.mli'
      ;;
    staged)
      git diff -z --cached --name-only --diff-filter=ACMR -- '*.ml' '*.mli'
      ;;
    base)
      git diff -z --name-only --diff-filter=ACMR "${BASE}...HEAD" -- '*.ml' '*.mli'
      ;;
    all)
      git ls-files -z -- '*.ml' '*.mli'
      ;;
  esac
}

tmp="$(mktemp)"
trap 'rm -f "$tmp"' EXIT
collect | sort -zu > "$tmp"

if [ ! -s "$tmp" ]; then
  echo "Nothing to format (no matching .ml/.mli files for scope '$MODE')."
  exit 0
fi

count="$(tr -cd '\0' < "$tmp" | wc -c | tr -d ' ')"

# --- run ocamlformat --------------------------------------------------------
if [ "$CHECK" -eq 1 ]; then
  if xargs -0 ocamlformat --check < "$tmp"; then
    echo "OK: all $count file(s) already formatted."
  else
    echo "FAIL: the file(s) above are not formatted. Run scripts/format.sh to fix." >&2
    exit 1
  fi
else
  xargs -0 ocamlformat --inplace < "$tmp"
  echo "Formatted $count file(s)."
fi

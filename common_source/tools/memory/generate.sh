#!/usr/bin/env bash
# Run fypp to regenerate all .F90 files in this directory.
#
# Files that are fypp includes (not top-level templates) are excluded:
#   my_alloc_impl_core.fy  — included by my_alloc_impl_idx4.fy and my_alloc_impl_idx8.fy
#   my_alloc.fy            — redirect note only, no longer generates a .F90
#
# Usage:
#   ./generate.sh          # regenerate all
#   ./generate.sh --dry-run  # print commands without executing

set -euo pipefail

DRY_RUN=false
if [[ "${1:-}" == "--dry-run" ]]; then
  DRY_RUN=true
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Top-level .fy templates: maps source -> output
declare -A TEMPLATES
TEMPLATES["my_alloc_impl_idx4.fy"]="my_alloc_impl_idx4.F90"
TEMPLATES["my_alloc_impl_idx8.fy"]="my_alloc_impl_idx8.F90"
TEMPLATES["my_dealloc.fy"]="my_dealloc.F90"
TEMPLATES["my_move_alloc.fy"]="my_move_alloc.F90"

if ! command -v fypp &>/dev/null; then
  echo "ERROR: fypp not found in PATH" >&2
  exit 1
fi

ERRORS=0
for SRC in "${!TEMPLATES[@]}"; do
  OUT="${TEMPLATES[$SRC]}"
  if $DRY_RUN; then
    echo "fypp $SRC $OUT"
  else
    echo "  fypp $SRC -> $OUT"
    if ! fypp "$SRC" "$OUT"; then
      echo "ERROR: fypp failed on $SRC" >&2
      ERRORS=$((ERRORS + 1))
    fi
  fi
done

if [[ $ERRORS -gt 0 ]]; then
  echo "FAILED: $ERRORS template(s) could not be processed." >&2
  exit 1
fi

$DRY_RUN || echo "Done."

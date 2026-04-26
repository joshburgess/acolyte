#!/usr/bin/env bash
set -euo pipefail

# Compile-time benchmark for acolyte.
# Measures how compile time scales with the number of API endpoints.

TARGETS=("bench-1" "bench-4" "bench-8" "bench-16" "bench-32")
SIZES=(1 4 8 16 32)

echo "=== acolyte compile-time benchmark ==="
echo ""
echo "Cleaning build artifacts..."
cabal clean 2>/dev/null || true
echo ""

# Build dependencies first so we only measure the benchmark module itself.
echo "Pre-building dependencies..."
cabal build --only-dependencies bench-1 2>&1 | tail -1
echo ""

declare -a TIMES

for i in "${!TARGETS[@]}"; do
    target="${TARGETS[$i]}"
    size="${SIZES[$i]}"

    # Clean just this target's build artifacts to force recompilation.
    # We rely on cabal's incremental build: dependencies are cached,
    # only the benchmark module itself is recompiled.
    find dist-newstyle -path "*/${target}/*" -name "*.o" -delete 2>/dev/null || true
    find dist-newstyle -path "*/${target}/*" -name "*.hi" -delete 2>/dev/null || true
    find dist-newstyle -path "*/compile-bench-*/${target}/*" -type f -delete 2>/dev/null || true

    printf "Building %-10s (%2d endpoints) ... " "$target" "$size"

    start=$(date +%s%N 2>/dev/null || python3 -c 'import time; print(int(time.time()*1e9))')
    cabal build "$target" 2>&1 >/dev/null
    end=$(date +%s%N 2>/dev/null || python3 -c 'import time; print(int(time.time()*1e9))')

    elapsed_ms=$(( (end - start) / 1000000 ))
    elapsed_s=$(printf "%.2f" "$(echo "scale=2; $elapsed_ms / 1000" | bc)")

    TIMES[$i]="$elapsed_s"
    echo "${elapsed_s}s"
done

echo ""
echo "=== Results ==="
echo ""
printf "%-12s  %10s  %10s\n" "Endpoints" "Time (s)" "Target"
printf "%-12s  %10s  %10s\n" "---------" "--------" "------"
for i in "${!TARGETS[@]}"; do
    printf "%-12s  %10s  %10s\n" "${SIZES[$i]}" "${TIMES[$i]}" "${TARGETS[$i]}"
done
echo ""

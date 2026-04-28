#!/usr/bin/env bash
set -euo pipefail

# Compile-time benchmark for acolyte vs Servant.
# Two API shapes:
#   - "trivial":  "pathN" :> Get '[JSON] Text
#   - "rich":     "resN"  :> Capture "id" Int :> ReqBody '[JSON] Body :> Post '[JSON] Resp
# Each shape is timed at 1, 4, 8, 16, and 32 endpoints under both libraries.

ACOLYTE_TRIVIAL=("bench-1" "bench-4" "bench-8" "bench-16" "bench-32")
SERVANT_TRIVIAL=("servant-bench-1" "servant-bench-4" "servant-bench-8" "servant-bench-16" "servant-bench-32")
ACOLYTE_RICH=("rich-1" "rich-4" "rich-8" "rich-16" "rich-32")
SERVANT_RICH=("servant-rich-1" "servant-rich-4" "servant-rich-8" "servant-rich-16" "servant-rich-32")
SIZES=(1 4 8 16 32)

now_ns() {
    date +%s%N 2>/dev/null || python3 -c 'import time; print(int(time.time()*1e9))'
}

time_target() {
    local target="$1"
    local size="$2"

    find dist-newstyle -path "*/${target}/*" -name "*.o" -delete 2>/dev/null || true
    find dist-newstyle -path "*/${target}/*" -name "*.hi" -delete 2>/dev/null || true
    find dist-newstyle -path "*/compile-bench-*/${target}/*" -type f -delete 2>/dev/null || true

    printf "  %-22s (%2d endpoints) ... " "$target" "$size"

    local start end elapsed_ms elapsed_s
    start=$(now_ns)
    cabal build "$target" 2>&1 >/dev/null
    end=$(now_ns)

    elapsed_ms=$(( (end - start) / 1000000 ))
    elapsed_s=$(printf "%.2f" "$(echo "scale=2; $elapsed_ms / 1000" | bc)")

    echo "${elapsed_s}s"
    printf "%s" "$elapsed_s" > /tmp/.compile_bench_last
}

run_group() {
    local label="$1"
    shift
    local -a targets=("$@")
    local -a results=()

    echo "--- $label ---"
    for i in "${!targets[@]}"; do
        time_target "${targets[$i]}" "${SIZES[$i]}"
        results+=("$(cat /tmp/.compile_bench_last)")
    done
    echo ""

    # Emit results into named globals via printf (bash 3 friendly)
    printf '%s\n' "${results[@]}"
}

ALL_TARGETS=("${ACOLYTE_TRIVIAL[@]}" "${SERVANT_TRIVIAL[@]}" "${ACOLYTE_RICH[@]}" "${SERVANT_RICH[@]}")

echo "=== compile-time benchmark: acolyte vs Servant ==="
echo ""
echo "Cleaning build artifacts..."
cabal clean 2>/dev/null || true
echo ""

echo "Pre-building dependencies (this may take a while the first time)..."
cabal build --only-dependencies "${ALL_TARGETS[@]}" 2>&1 | tail -1
echo ""

declare -a A_TRIVIAL_TIMES S_TRIVIAL_TIMES A_RICH_TIMES S_RICH_TIMES

echo "=== Trivial API: \"pathN\" :> Get '[JSON] Text ==="
echo ""
echo "--- acolyte ---"
for i in "${!ACOLYTE_TRIVIAL[@]}"; do
    time_target "${ACOLYTE_TRIVIAL[$i]}" "${SIZES[$i]}"
    A_TRIVIAL_TIMES+=("$(cat /tmp/.compile_bench_last)")
done
echo ""
echo "--- Servant ---"
for i in "${!SERVANT_TRIVIAL[@]}"; do
    time_target "${SERVANT_TRIVIAL[$i]}" "${SIZES[$i]}"
    S_TRIVIAL_TIMES+=("$(cat /tmp/.compile_bench_last)")
done
echo ""

echo "=== Rich API: \"resN\" :> Capture Int :> ReqBody Body :> Post Resp ==="
echo ""
echo "--- acolyte ---"
for i in "${!ACOLYTE_RICH[@]}"; do
    time_target "${ACOLYTE_RICH[$i]}" "${SIZES[$i]}"
    A_RICH_TIMES+=("$(cat /tmp/.compile_bench_last)")
done
echo ""
echo "--- Servant ---"
for i in "${!SERVANT_RICH[@]}"; do
    time_target "${SERVANT_RICH[$i]}" "${SIZES[$i]}"
    S_RICH_TIMES+=("$(cat /tmp/.compile_bench_last)")
done
echo ""

echo "=== Results ==="
echo ""
echo "Trivial: \"pathN\" :> Get '[JSON] Text"
printf "%-12s  %12s  %12s\n" "Endpoints" "Servant (s)" "acolyte (s)"
printf "%-12s  %12s  %12s\n" "---------" "-----------" "-----------"
for i in "${!SIZES[@]}"; do
    printf "%-12s  %12s  %12s\n" "${SIZES[$i]}" "${S_TRIVIAL_TIMES[$i]}" "${A_TRIVIAL_TIMES[$i]}"
done
echo ""
echo "Rich: \"resN\" :> Capture Int :> ReqBody Body :> Post Resp"
printf "%-12s  %12s  %12s\n" "Endpoints" "Servant (s)" "acolyte (s)"
printf "%-12s  %12s  %12s\n" "---------" "-----------" "-----------"
for i in "${!SIZES[@]}"; do
    printf "%-12s  %12s  %12s\n" "${SIZES[$i]}" "${S_RICH_TIMES[$i]}" "${A_RICH_TIMES[$i]}"
done
echo ""
echo "Note: wall-clock numbers include GHC startup, linking, and dependency"
echo "loading. The interesting signal is how each column changes with"
echo "endpoint count and combinator richness."
echo ""

rm -f /tmp/.compile_bench_last

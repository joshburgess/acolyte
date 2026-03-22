#!/bin/bash
# Compile-time benchmark: measure how compilation scales with API size.
#
# Generates benchmark modules with 5, 10, 20, 40, 80 endpoints
# and times the compilation of each.
#
# Usage: cd benchmarks/compile-time && ./run-benchmarks.sh

set -e

# Generate the modules
runghc GenBenchmark.hs

echo ""
echo "=== Compile-Time Scaling Benchmark ==="
echo "Measuring compilation time for APIs of increasing size."
echo "If our approach is correct, time should scale linearly (not exponentially)."
echo ""

for N in 5 10 20 40 80; do
  # Clean previous compilation artifacts
  rm -f Bench${N}.hi Bench${N}.o Bench${N}.dyn_hi Bench${N}.dyn_o

  # Time the compilation
  echo -n "  ${N} endpoints: "
  TIME=$( { time cabal exec ghc -- -c Bench${N}.hs -no-link 2>&1; } 2>&1 | grep real | awk '{print $2}' )

  # Fallback: use /usr/bin/time if time doesn't produce 'real'
  if [ -z "$TIME" ]; then
    START=$(date +%s%N 2>/dev/null || python3 -c 'import time; print(int(time.time()*1000))')
    cabal exec ghc -- -c Bench${N}.hs -no-link 2>/dev/null
    END=$(date +%s%N 2>/dev/null || python3 -c 'import time; print(int(time.time()*1000))')
    if [ ${#START} -gt 13 ]; then
      # nanoseconds
      MS=$(( (END - START) / 1000000 ))
    else
      # milliseconds
      MS=$(( END - START ))
    fi
    echo "${MS}ms"
  else
    echo "$TIME"
  fi
done

echo ""
echo "Linear scaling = our type-level approach works."
echo "Exponential scaling = we have the same problem as Servant."

# Clean up
rm -f Bench*.hi Bench*.o Bench*.dyn_hi Bench*.dyn_o

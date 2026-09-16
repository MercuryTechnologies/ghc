#!/usr/bin/env bash
set -euo pipefail

GHC="${GHC:-$(dirname "$0")/../_build/stage1/bin/ghc}"
DIR="$(cd "$(dirname "$0")" && pwd)"

cd "$DIR"

echo "============================================"
echo "GHC Generic deriving: STUB bindings (-fds-generic-cheap)"
echo "============================================"
echo "Using: $GHC"
echo ""

for f in Sum10.hs Sum25.hs Sum50.hs Sum100.hs Sum200.hs \
         Rec10.hs Rec25.hs Rec50.hs Rec100.hs Rec200.hs \
         SumRec10x5.hs SumRec25x5.hs SumRec50x5.hs \
         SumRec10x10.hs SumRec25x10.hs SumRec50x10.hs; do

    mod="${f%.hs}"
    rm -f "$mod.hi" "$mod.o"

    printf "%-25s" "$mod"

    times=()
    for run in 1 2 3; do
        rm -f "$mod.hi" "$mod.o"
        elapsed=$( { TIMEFORMAT='%3R'; time "$GHC" -fforce-recomp -fds-generic-cheap -O0 -c "$f" 2>/dev/null; } 2>&1 )
        times+=("$elapsed")
    done
    echo "  ${times[0]}s  ${times[1]}s  ${times[2]}s"
done

echo ""
echo "Done. (3 runs per module, times in seconds)"

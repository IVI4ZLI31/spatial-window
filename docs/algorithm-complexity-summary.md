# Algorithm Complexity + LOC Summary

Updated: 2026-02-12
Branch reference: `codex/algorithm-analysis`
Current default algorithm: row-gated Voronoi (`spatial-window-geometry.el`)

**Variables**
- `R` = keyboard rows
- `C` = keyboard columns
- `K = R*C` = total key cells
- `W` = windows
- `I` = rebalance iterations (EMD/transport variant; capped at 200 in code)

## LOC (excluding comments and strings)

| File | LOC |
| --- | --- |
| `/Users/lewang/.le-emacs.d/var/elpaca/repos/spatial-window/spatial-window-geometry.el` | 270 |
| `/Users/lewang/.le-emacs.d/var/elpaca/repos/spatial-window/spatial-window-geometry-simple.el` | 270 |
| `/Users/lewang/.le-emacs.d/var/elpaca/repos/spatial-window/spatial-window-geometry-hungarian.el` | 195 |
| `/Users/lewang/.le-emacs.d/var/elpaca/repos/spatial-window/spatial-window-geometry-emd.el` | 197 |
| `/Users/lewang/.le-emacs.d/var/elpaca/repos/spatial-window/spatial-window-geometry-voronoi.el` | 153 |

## Asymptotic Complexity

| Algorithm | Dominant Steps | Worst-case Complexity | Notes |
| --- | --- | --- | --- |
| Original | assign-cells + ensure/steal + consolidation | `O(K * W^2)` | Typical `O(K * W)`; consolidation introduces `W^2` term in worst case. |
| Simple (current) | same as original | `O(K * W^2)` | Same behavior as original to satisfy existing tests. |
| Hungarian | cost matrix + rectangular Hungarian + fill | `O(W^2 * K)` | Global assignment has heavier constants than greedy passes. |
| EMD/Transport | assign-cells + ensure + rebalance | `O(I * K * W)` | Linear in `W`, but rebalance has large constant; `I` capped. |
| Voronoi | centroid assignment + ensure | `O(K * W)` | Lowest asymptotic complexity. |

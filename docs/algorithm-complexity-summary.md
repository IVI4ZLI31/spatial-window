# Algorithm Complexity + LOC Summary

Updated: 2026-02-12
Branch reference: `voronoi-row-try2`
Current default algorithm: row-gated Voronoi (`spatial-window-geometry.el`)

**Variables**
- `R` = keyboard rows
- `C` = keyboard columns
- `K = R*C` = total key cells
- `W` = windows
- `I` = rebalance iterations (EMD/transport variant; capped at 200 in code)

## LOC (excluding comments and strings)

| File | LOC | Branch |
| --- | --- | --- |
| `spatial-window-geometry.el` (row-gated Voronoi) | ~270 | current |
| `spatial-window-geometry-simple.el` | 270 | `codex/algorithm-analysis` |
| `spatial-window-geometry-hungarian.el` | 195 | `codex/algorithm-analysis` |
| `spatial-window-geometry-emd.el` | 197 | `codex/algorithm-analysis` |
| `spatial-window-geometry-voronoi.el` | 153 | `codex/algorithm-analysis` |

## Asymptotic Complexity

| Algorithm | Dominant Steps | Worst-case Complexity | Notes |
| --- | --- | --- | --- |
| Row-gated Voronoi (current) | assign-cells + assign-edges + ensure/steal | `O(K * W)` | Lowest asymptotic complexity. Edge pass is `O(C * W)`. |
| Original (overlap) | assign-cells + ensure/steal + consolidation | `O(K * W^2)` | Typical `O(K * W)`; consolidation introduces `W^2` term in worst case. |
| Simple | same as original | `O(K * W^2)` | Same behavior as original to satisfy existing tests. |
| Hungarian | cost matrix + rectangular Hungarian + fill | `O(W^2 * K)` | Global assignment has heavier constants than greedy passes. |
| EMD/Transport | assign-cells + ensure + rebalance | `O(I * K * W)` | Linear in `W`, but rebalance has large constant; `I` capped. |
| Voronoi (ungated) | centroid assignment + ensure | `O(K * W)` | Same complexity as row-gated but no row gating or edge affinity. |

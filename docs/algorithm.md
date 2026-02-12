# Algorithm: Row-Gated Voronoi Mapping

This document describes the current key assignment algorithm used by
`spatial-window`. The implementation lives in `spatial-window-geometry.el`.

The algorithm maps keyboard positions to windows based on a row-gated,
area-weighted Voronoi assignment with a reachability guarantee.

## Goals

- Preserve spatial intuition: keys should map to windows in the same relative
  screen position.
- Encourage rectangular key regions, especially across rows.
- Ensure every window remains reachable by at least one key.
- Keep the algorithm simple, deterministic, and fast.

## Inputs

- **Window bounds**: list of `(window x-start x-end y-start y-end)`, normalized
  to `[0,1]` in frame coordinates.
- **Keyboard layout**: list of rows, e.g. 3x10 for QWERTY.

## Definitions

- **Cell**: a keyboard grid cell at `(row, col)` with normalized bounds:
  - `x = [col/cols, (col+1)/cols]`
  - `y = [row/rows, (row+1)/rows]`
- **Centroid** of window `w`: `( (x0+x1)/2 , (y0+y1)/2 )`
- **Area** of window `w`: `(x1-x0) * (y1-y0)`
- **Row overlap fraction** for window `w` and cell row `r`:
  - `overlap_y = overlap(cell_y_range, win_y_range) / cell_height`

## Algorithm Overview

1. **Row-gated Voronoi assignment**
2. **Reachability guarantee (steal pass)**
3. **Convert grid to key lists**

### 1) Row-Gated Voronoi Assignment

For each grid cell:

1. Compute the set of **eligible windows**:
   - A window is eligible if its vertical overlap with the cell’s row band is
     at least `spatial-window--row-overlap-min` (currently `0.6`).
   - If no windows meet the threshold, fall back to all windows.

2. For each eligible window `w`, compute the **weighted distance** from the
   cell center `(cx, cy)` to the window centroid `(wx, wy)`:

```
dist = (cx - wx)^2 + (cy - wy)^2
score = dist / max(area(w), 0.001)
```

3. Assign the cell to the window with the **minimum score**.

This is a Voronoi-like partition, weighted by window area, while the row gate
prevents cross-row leakage and favors rectangular bands.

### 2) Reachability Guarantee (Steal Pass)

After the initial assignment, some windows might still have zero keys (e.g.
very thin panels). The algorithm ensures **every window has at least one key**:

- For each keyless window, find the cell with **maximum overlap** with that
  window, as long as:
  - The cell is unassigned, or
  - Its current owner has more than one key.
- Assign that cell to the keyless window.
- Repeat until all windows have at least one key.

This keeps the algorithm predictable while guaranteeing reachability.

### 3) Convert Grid to Keys

Finally, the assigned grid cells are mapped to the keyboard layout, producing
`(window . keys)` pairs for display and selection.

## Complexity

Let `K = rows * cols` and `W = number of windows`.

- Row-gated Voronoi assignment: `O(K * W)`
- Steal pass (worst case): `O(K * W)`
- Overall: `O(K * W)`

## Behavior Summary

- **Spatially coherent**: keys track the screen position of windows.
- **Row-stable**: row gating discourages mixed-row ownership.
- **Reachable**: every window gets at least one key.

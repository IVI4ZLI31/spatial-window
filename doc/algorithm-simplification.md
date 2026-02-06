# Key Assignment Algorithm Simplification

## Summary

Replaced the 6-step bounding-box pipeline (~200 lines, 8 functions, 2 thresholds) with a margin-based
assignment algorithm (~65 lines, 3 new functions, 1 threshold). The new algorithm produces equal or better
results: more keys assigned in ambiguous layouts, identical results for clean geometries.

## Old Algorithm (6-step pipeline)

1. **Compute all overlaps** — build 2D matrix of (cell, window) overlap fractions
2. **Two-phase ownership** — Phase 1: cells with >75% overlap claimed immediately. Phase 2: remaining cells go to
   "needy" windows (those without any strong ownership) if they beat competitors by >20% margin
3. **Extract bounding boxes** — compute min/max row/col rectangle for each window's owned cells
4. **Resolve box overlaps** — where rectangles overlap, highest-overlap window wins
5. **Steal for keyless windows** — iterate: find best stealable cell, recalculate donor's bounding box
6. **Convert to keys** — map grid cells to key strings

The bounding-box machinery (steps 3-4, ~120 lines) existed to enforce rectangular key regions. This doesn't
matter for UX because users see the overlay showing exactly which keys map to each window.

## New Algorithm (3 steps)

1. **`assign-cells`** — For each cell, compute overlap with all windows. Assign to best window if
   `best - second_best > 0.05` margin. Otherwise leave nil (ambiguous).
2. **`ensure-all-windows-have-keys`** — For each window with 0 keys: find cell with highest overlap where
   current owner has >1 key. Steal it. Uses count hash table for O(1) lookups instead of O(n×m) rescans.
   Iterates until convergence.
3. **`final-to-keys`** — unchanged from old algorithm.

### Why 0.05 margin?

The margin needs to be small enough to assign cells that clearly belong to one window (e.g., a cell 60%/40%
split should go to the 60% window) but large enough to leave truly ambiguous cells unassigned (e.g., 50%/50%
splits at window boundaries). 0.05 (5%) achieves this: a 52.5%/47.5% split is assigned, a 50%/50% split is
not.

## Before/After Comparison

### Test Cases With Identical Results

These layouts have clean geometry where cells fall entirely within one window:

| Test Case | Keys Assigned | Notes |
|-----------|:------------:|-------|
| single-window | 30/30 | Trivial: one window gets all keys |
| 2-columns | 30/30 | Clean 50/50 vertical split |
| 2-left-1-right | 25/30 | Middle row left side unassigned (50/50 vertical tie) |
| 3-columns | 25/30 | Middle row center unassigned (50/50 vertical tie) |
| max-3-rows | 30/30 | Clean 33/33/33 horizontal split |
| max-10-cols | 30/30 | Clean 10×10% vertical splits |

### Test Cases With Changed Results

#### extreme-split (95.5% main / 4.5% sidebar)

```
Window layout:              Keyboard (3×10):
┌─────────────────────┬──┐
│                     │  │  OLD: main=27  sidebar-top=2(p,;)  sidebar-bot=1(/)
│      main 95.5%     ├──┤  NEW: main=28  sidebar-top=1(p)    sidebar-bot=1(/)
│                     │  │
└─────────────────────┴──┘  ";" now goes to main (wins by margin) instead of
                    4.5%    being given to sidebar-top via Phase 2 needy logic
```

OLD grid (30/30 assigned):
```
q  w  e  r  t  y  u  i  o  p     ← p = sidebar-top
a  s  d  f  g  h  j  k  l  ;     ← ; = sidebar-top
z  x  c  v  b  n  m  ,  .  /     ← / = sidebar-bot
```

NEW grid (30/30 assigned):
```
q  w  e  r  t  y  u  i  o  p     ← p = sidebar-top (stolen)
a  s  d  f  g  h  j  k  l  ;     ← ; = main (wins by margin)
z  x  c  v  b  n  m  ,  .  /     ← / = sidebar-bot (stolen)
```

#### complex-spanning (7 windows)

```
Window layout:
┌───────────────────┬─────────┐
│      magit        │         │
│       48%         │         │
├──┬──┬────┬────────┤  claude │
│s1│s2│ s3 │   s4   │  100%   │
├──┼──┤    │        │         │
│bt│  │    │        │         │
└──┴──┴────┴────────┴─────────┘
       51%              49%
```

OLD grid (28/30 assigned):
```
q  w  e  r  t  y  u  i  o  p     magit=5(q,w,e,r,t)  claude=15(right half)
a  ·  s  d  ·  h  j  k  l  ;     sw1=1(a) sw3=3(s,d,c) sw2=1(x)
z  x  c  v  b  n  m  ,  .  /     sw4=2(b,v) backtrace=1(z)
```

NEW grid (30/30 assigned):
```
q  w  e  r  t  y  u  i  o  p     magit=7(q,w,e,r,t,s,d)  claude=15(right half)
a  ·  ·  f  g  h  j  k  l  ;     sw1=1(a) sw4=4(b,f,g,v) sw2=1(x)
z  x  c  v  b  n  m  ,  .  /     sw3=1(c) backtrace=1(z)
```

Magit gets `s,d` from middle row (strong overlap). sw4 gets `f,g` (spatial position matches).
sw3 shrinks from 3 to 1 key — acceptable since it's a very narrow window.

#### ide-layout-thin-panel (main + diff + claude)

```
Window layout:
┌─────────────┬───────┐
│             │       │
│  main 93%   │claude │
│             │ 100%  │
├─────────────┤       │
│  diff 5%    │       │
└─────────────┴───────┘
     63%         37%
```

OLD grid (27/30 assigned):
```
q  w  e  r  t  y  ·  i  o  p     main=17  diff=1(v)  claude=9
a  s  d  f  g  h  ·  k  l  ;     col 6 (u,j,m) unmapped — boundary ambiguity
z  x  c  v  b  n  ·  ,  .  /
```

NEW grid (30/30 assigned):
```
q  w  e  r  t  y  u  i  o  p     main=17  diff=1(v)  claude=12
a  s  d  f  g  h  j  k  l  ;     col 6 (u,j,m) now assigned to claude
z  x  c  v  b  n  m  ,  .  /     (claude wins by margin at the 63% boundary)
```

#### extreme-narrow-left (4% left / 96% right)

```
Window layout:
┌──┬────────────────────────────┐
│  │                            │
│4%│         right 96%          │
├──┤                            │
│4%│                            │
└──┴────────────────────────────┘
```

OLD grid (30/30 assigned):
```
q  w  e  r  t  y  u  i  o  p     top-left=2(q,a)  bot-left=1(z)  right=27
a  s  d  f  g  h  j  k  l  ;     "q","a" given to top-left via Phase 2
z  x  c  v  b  n  m  ,  .  /
```

NEW grid (30/30 assigned):
```
q  w  e  r  t  y  u  i  o  p     top-left=1(a)  bot-left=1(z)  right=28
a  s  d  f  g  h  j  k  l  ;     top-left steals only "a" (highest overlap)
z  x  c  v  b  n  m  ,  .  /     "q" stays with right (no Phase 2 to give it away)
```

#### misaligned-vertical-splits (60/40 top, 33/67 bottom)

```
Window layout:
┌────────────────────────┬────────────────┐
│      top-left (60%)    │  top-right 40% │
├───────────┬────────────┴────────────────┤
│ bot-left  │      bot-right (67%)        │
│   (33%)   │                             │
└───────────┴─────────────────────────────┘
```

OLD grid (19/30 assigned):
```
q  w  e  r  t  y  u  i  o  p     TL=6  TR=4  BL=3  BR=6
·  ·  ·  ·  ·  ·  ·  ·  ·  ·     entire middle row unmapped (ambiguous)
z  x  c  ·  b  n  m  ,  .  /
```

NEW grid (21/30 assigned):
```
q  w  e  r  t  y  u  i  o  p     TL=7  TR=4  BL=3  BR=7
·  ·  ·  f  ·  ·  ·  ·  ·  ·     "f" assigned to TL, "v" to BR
z  x  c  v  b  n  m  ,  .  /     (these cells have clear overlap advantage)
```

#### misaligned-splits-edge (59/41 top, 32/68 bottom)

Same pattern as above. OLD: 20/30. NEW: 21/30. Middle row cell "f" assigned to top-left, "v" to bot-right.

#### real-dev-session (5 windows)

```
Window layout:
┌────┬─────────────────────────────┐
│code│                             │
│nar │      code-wide (89%)        │
│11% │                             │
├────┼─────────────────────────────┤
│    │     posframe-top (68%)      │
│mag ├─────────────────────────────┤
│32% │     posframe-bot (68%)      │
└────┴─────────────────────────────┘
```

OLD grid (27/30 assigned):
```
q  w  e  r  t  y  u  i  o  p     narrow=1(q)  wide=9  magit=3(z,x,c)
·  ·  ·  f  g  h  j  k  l  ;     posframe-top=7(f,g,h,j,k,l,;)
z  x  c  v  b  n  m  ,  .  /     posframe-bot=7  (a,s,d unassigned)
```

NEW grid (29/30 assigned):
```
q  w  e  r  t  y  u  i  o  p     narrow=1(q)  wide=9  magit=6(a,s,d,z,x,c)
a  s  d  ·  g  h  j  k  l  ;     posframe-top=6(g,h,j,k,l,;)
z  x  c  v  b  n  m  ,  .  /     posframe-bot=7  ("f" unassigned)
```

Magit picks up `a,s,d` from middle row (clear overlap from its 32% width spanning rows 2-3). Only `f` remains
unassigned — it's at the boundary between magit and posframe-top.

## Summary Table

| Test Case | OLD assigned | NEW assigned | Delta | Changed windows |
|-----------|:-----------:|:-----------:|:-----:|-----------------|
| single-window | 30 | 30 | 0 | — |
| 2-columns | 30 | 30 | 0 | — |
| 2-left-1-right | 25 | 25 | 0 | — |
| 3-columns | 25 | 25 | 0 | — |
| max-3-rows | 30 | 30 | 0 | — |
| max-10-cols | 30 | 30 | 0 | — |
| extreme-split | 30 | 30 | 0 | sidebar-top: 2→1, main: 27→28 |
| complex-spanning | 28 | 30 | +2 | magit: 5→7, sw4: 2→4, sw3: 3→1 |
| ide-layout-thin-panel | 27 | 30 | +3 | claude: 9→12 |
| extreme-narrow-left | 30 | 30 | 0 | top-left: 2→1, right: 27→28 |
| misaligned-vertical | 19 | 21 | +2 | TL: 6→7, BR: 6→7 |
| misaligned-splits-edge | 20 | 21 | +1 | TL: 6→7 |
| real-dev-session | 27 | 29 | +2 | magit: 3→6, posframe-top: 7→6 |

## Trade-offs

**Gains:**
- 288 lines → 124 lines (net -164 lines, 57% reduction)
- 8 deleted functions, 3 new functions
- Single threshold (0.05) vs two thresholds (0.75 + 0.20)
- More keys assigned in ambiguous layouts (up to +3 per case)
- Simpler steal logic: O(1) count lookups via hash table vs O(n×m) grid rescans

**Losses:**
- Key regions are no longer guaranteed rectangular (but this was invisible to users)
- "Needy window" Phase 2 logic removed — tiny windows that previously got extra cells via the
  `has-strong-ownership` filter now only get cells through the steal mechanism
- In extreme-narrow-left, top-left gets 1 key instead of 2 (but it's a 4% wide window)

**Neutral:**
- Clean geometries (clean splits, no ambiguity) produce identical results
- All windows still guaranteed ≥1 key

# generate

Personal R package of helper functions for algorithmic art. Provides shared primitives (coordinate transforms, noise functions, graph operations, rendering, sampling, simulation) used across all `gen_*` generative art repositories.

## Global Organization

The codebase follows a three-tier tool hierarchy:

1. **Generic (`{generate}` package)** - **This repository.** Shared functions across all systems. Versioned and distributed as an R package.
2. **Systems (`gen_*` repositories)** - System-specific functions organized around: **get features** -> **generate data** -> **render data**. Versioned but not packaged.
3. **Series** - Style variations using subsets of system-level code, often symlinked. Features appearing across multiple series migrate upward to the package.

Functions start in system-level notebooks, then migrate here when reused across systems.

## Package Structure

```
R/
├── system_attractors.R   # Strange attractors (logistic, quadratic, Henon maps)
├── system_gis.R          # DEM reading, ridge/contour rendering
├── system_graph.R        # Graph-based rendering (RNG, KNN, MST)
├── system_ode.R          # ODE simulations (Lorenz, Duffing, Lotka-Volterra)
├── system_sequence.R     # Collatz-based organic structures (leaf, node)
├── system_splines.R      # Text/glyph generation via B-splines and vector fonts
├── utils_curves.R        # Mapping functions (sigmoid, exponential, square)
├── utils_files.R         # Plot export (save_plot)
├── utils_geometry.R      # 2D transforms, bounding boxes, sf conversion
├── utils_globals.R       # Global variable declarations (R CMD CHECK)
├── utils_imports.R       # Magrittr pipe re-export
├── utils_partition.R     # Partitioning and group indexing
├── utils_rendering.R     # Paper texture, custom ggplot2 theme
├── utils_sampling.R      # 1D/2D sampling (Halton, Fermat spiral, LHS)
man/                      # Roxygen2-generated documentation
```

### System Modules

**Attractors** (`system_attractors.R`) - Discrete-time dynamical systems. Implements Sprott's method for automatic strange attractor generation via letter-to-parameter encoding. Includes Lyapunov exponent computation for chaos detection and text rendering using attractor glyphs.

**GIS** (`system_gis.R`) - Reads IGN elevation data (DBALTI 25m, RGEALTI 5m). Renders terrain as ridge plots with perspective occlusion or contour maps with waterlines. Multiple filtering methods for ridgeline cleaning (length, slope, rank).

**Graph** (`system_graph.R`) - Renders point layouts using graph algorithms (RNG, KNN, MST) with five aesthetic modes (default, line, quadratic Bezier, cubic Bezier, arc). Uses cccd/tidygraph/ggraph.

**ODE** (`system_ode.R`) - Continuous-time simulations (Lorenz, Duffing, Lotka-Volterra) via deSolve, plus discrete quadratic maps. Field generation with LHS-sampled initial conditions and parallel execution via furrr.

**Sequence** (`system_sequence.R`) - Organic structures from Collatz sequences. Generates leaf/node patterns with spiral and wave shapes. Four path rendering methods (spline, path, polygon, polygon_lm).

**Splines** (`system_splines.R`) - Text generation pipeline: `gen_sequence` -> `gen_charmap` -> `layout_paragraph` -> `render_spline`. Supports B-spline glyphs and Hershey vector fonts. Four text orientations (lrtb, rltb, tbrl, tblr). Character spacing, overflow, and whitespace control.

### Utility Modules

**Curves** - Sigmoid, exponential, and discretization functions for non-linear mapping.

**Files** - `save_plot()` with format presets: png (400 dpi), jpg (218 dpi), snap (150 dpi), svg. Default A5 size, warm off-white background (#FEFAEE).

**Geometry** - 2D transforms (`tr_rotate`, `tr_translate`, `tr_jitter`, `tr_wave`, `tr_loess`, `tr_recurse`), bounding boxes, sf conversion, rectangular buffers, frame rendering.

**Partition** - Unit partitioning via fraction combinations, group indexing (fixed/quantile/random/hclust), rounding utilities.

**Rendering** - `gen_paper()` for fiber-based texture (30k diagonal lines). `theme_paper()` for digital/plotter output modes.

**Sampling** - Space-filling designs: Halton sequences, Fermat spiral, uniform/gaussian ellipse sampling, perimeter sampling. Local extrema detection. Noise injection.

## Naming Conventions

| Prefix | Purpose | Example |
|--------|---------|---------|
| `gen_*` | Generate data or plots | `gen_charmap()`, `gen_paper()` |
| `render_*` | Render ggplot objects | `render_spline()`, `render_ridge()` |
| `layout_*` | Arrange elements in space | `layout_paragraph()`, `layout_ellipse()` |
| `tr_*` | Transform coordinates | `tr_rotate()`, `tr_wave()` |
| `filter_*` | Filter data subsets | `filter_ridge_length()` |
| `sample_*` | Sample points or values | `sample_rectangle()`, `sample_letters()` |
| `f_*` | Mathematical functions | `f_sig()`, `f_exp()` |
| `l_*` | Local feature detection | `l_max()`, `l_min()` |
| `simulate_*` | ODE integration | `simulate_lorenz()` |
| `seq_*` | Sequence generation | `seq_collatz()`, `seq_noise()` |

## Dependencies

**Imports** (required): purrr, dplyr, tidyr, magrittr, ggplot2, ggforce

**Suggests** (optional, by system):
- Graph: igraph, tidygraph, ggraph, cccd
- GIS: sf, stars, units
- ODE: deSolve, lhs, furrr
- Sampling: randtoolbox, truncnorm, MASS
- Text: stringi, stringr, hershey
- Rendering: cowplot, colorspace, scales
- Partition: combinat
- Other: glue, tibble, tidyselect, RcppRoll, distr

## Coding Style

- **Tidyverse-centric**: dplyr, purrr, ggplot2, tibble
- **Pipe chains**: `|>` (base R pipe, migrated from `%>%`)
- **Functional mapping**: `map()`, `map2()`, `pmap()` for iteration
- **Switch dispatch**: method selection via `switch()` blocks
- **Roxygen2 documentation**: markdown-enabled, `@export` tags
- **No tests**: package is validated through use in downstream `gen_*` systems

## Output

- JPEG/PNG for digital preview
- SVG for pen plotting, post-processed via vpype (`linemerge`, `linesort`)

## Evolution

Major development phases, from git history (62 commits, Nov 2021 - Jan 2026):

### Phase 1 - Foundation (Nov 2021 - Mar 2022)

Initial package creation with core systems: attractors, GIS (ridge/contour rendering), graph rendering, ODE simulations, Collatz sequences, and spline-based text generation. Documented all systems and bumped to version 1.0.0.

### Phase 2 - Expansion (Apr - Oct 2022)

Extended existing systems: added MST and arc methods for graphs, perspective scaling for ridges, cubic Bezier curves, frame rendering (`get_box`, `sample_rectangle`, `render_frame`). Added `save_plot()`, color support in spline functions. Switched to base R pipe operator (`|>`).

### Phase 3 - Consolidation (Jan - Nov 2023)

Added theming (`theme_paper`), paper texture (`gen_paper`), pseudorandom sequence generators, and parallel simulation support. Merged sampling functions under `layout_*` prefix. Rewrote asemic text system in a more generic way, replacing cowplot with direct ggplot calls.

### Phase 4 - Text System Refinement (May 2024 - Feb 2025)

Focused on the spline/text system: character and word spacing control, whitespace handling, text overflow, partition/indexing utilities. Added vector font rendering support, periodic wave transformations for 2D objects. Replaced remaining R pipes with magrittr in legacy functions.

### Phase 5 - Utilities & Maintenance (Feb 2025 - Jan 2026)

Extended utilities: local extrema detection (`l_max`, `l_min`), line smoothing transformations (`tr_loess`), recursive transforms (`tr_recurse`). Updated for ggplot2 4.0 compatibility. Bug fixes in extrema functions, group indexing, and limit adjustment.

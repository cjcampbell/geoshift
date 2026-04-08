---
editor_options: 
  markdown: 
    wrap: 72
---
<img src="https://raw.githubusercontent.com/cjcampbell/geoshift/master/images/geoshift_hex1.png" alt="geoshift hex logo showing a globe with a red-cyan 3D effect" width="200" align="right"/>

# geoshift
**Metrics for quantifying seasonal range shifts from temporally-explicit
species distribution models**

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: CC0](https://img.shields.io/badge/License-CC0-lightgrey.svg)](https://creativecommons.org/publicdomain/zero/1.0/)
<!-- badges: end -->

------------------------------------------------------------------------

Understanding whether, how far, and how seasonally a species migrates is
fundamental to conservation planning, population ecology, and the study
of range dynamics under climate change. Yet tracking the full diversity
of migratory behavior across thousands of species remains logistically
and financially out of reach with individual-based methods alone.
`geoshift` takes a different approach: it extracts ecologically
interpretable metrics of migratory strategy directly from **pairs of
seasonal species distribution model (SDM) surfaces** — one representing
the breeding season and one representing the nonbreeding season. No
tracking data required.

------------------------------------------------------------------------

## Three core metrics geoshift looks to approximate

| Metric | What it captures | Range |
|---|---|---|
| **Centroid distance** | How far apart are the seasonal range centers? A proxy for migration distance (km). | 0 → ∞ |
| **Seasonality** | How much of the range is stable year-round versus seasonally distinct? | −1 (fully resident) → +1 (fully migratory) |
| **Range size change** | Does the species contract, expand, or maintain its range between seasons? (breeding:nonbreeding area ratio) | 0 → ∞ |

------------------------------------------------------------------------

## Installation

`geoshift` is currently available from GitHub:

```r
# install.packages("remotes")
remotes::install_github("cjcampbell/geoshift")
```

------------------------------------------------------------------------

For a demonstration on the functionality of geoshift, see the
[Get Started vignette](https://cjcampbell.github.io/geoshift/articles/geoshift.html).

------------------------------------------------------------------------

## Function overview

| Group | Functions |
|---|---|
| Metrics from continuous rasters | `schoenersD()`, `schoenersProjection()`, `calculateChangeAtPoints()`, `extractStatistics()` |
| Metrics from binary rasters & polygons | `extractPAstatistics()`, `statsFromPolygons()`, `areaOfExpectedOccurrence()` |
| Ellipse methods | `makeDataEllipse()`, `makeEllipses()` |
| Occurrence & coordinate utilities | `coordsToConvexHull()`, `coordsToKDEPolygon()`, `surface2df()` |
| Visualization | `makePlot()`, `makeCompoundPlot()` |

------------------------------------------------------------------------

## Citation

A companion manuscript describing and validating the methods implemented in
`geoshift` is currently in preparation. A citation and link will be provided
here upon publication.

In the meantime, if you use `geoshift` in your work, please cite the package
directly:

```r
citation("geoshift")
```

------------------------------------------------------------------------

## Authors

-   **CJ Campbell** — Bat Conservation International & University of
    Wisconsin–Madison (author)
-   **Michael Belitz** — Michigan State University & University of
    Wisconsin–Madison (contributor)

------------------------------------------------------------------------

## License

[CC0 1.0 Universal](https://creativecommons.org/publicdomain/zero/1.0/)
— dedicated to the public domain.

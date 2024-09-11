
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gbem <a href="https://stochagbem.github.io/gbem/"><img src="man/figures/logo.png" align="right" height="138" alt="gbem website" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/gbem)](https://CRAN.R-project.org/package=gbem)
[![R-CMD-check](https://github.com/stochaGBEM/gbem/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stochaGBEM/gbem/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/stochaGBEM/gbem/branch/main/graph/badge.svg)](https://app.codecov.io/gh/stochaGBEM/gbem?branch=main)
<!-- badges: end -->

The goal of **gbem** is to implement a gravel bed river bank erosion
model, calculating erosion volumes expected at a river’s cross section
given an event hydrograph.

**WARNING**: This package is under active development and is not
intended to be used yet.

## Installation

As gbem is not on CRAN, you can install it from GitHub by running the
following command in R:

``` r
remotes::install_github("stochaGBEM/gbem")
```

If this doesn’t work, you may have to install the remotes package with
the command `install.packages("remotes")`.

## Example

``` r
library(gbem)
library(sxchan)
library(sf)
#> Linking to GEOS 3.11.0, GDAL 3.5.3, PROJ 9.1.0; sf_use_s2() is TRUE
```

### Cross Sections

Make channel cross sections from the default bankline polygon in the
**sxchan** package, and add cross section features required for the
Manning erosion engine.

``` r
channel <- xt_generate_sxc(demo_bankline, n = 100) |> 
  sx_manning(
    grad = 0.02, d50 = 65, d84 = 100, 
    roughness = seq(0.001, 0.01, length.out = 100)
  )
```

Here is what they look like.

``` r
plot(demo_bankline)
plot(st_geometry(channel), add = TRUE)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

What’s the largest flow that the channel can accommodate without
eroding? Visualize in a plot.

``` r
eroding_flow(channel, resistance = "manning") |> 
  plot(ylab = "Flow", xlab = "Cross Section Number")
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

Or, the reverse can be calculated: what’s the minimum channel width that
can accommodate a flow without eroding? Try a flow of 200:

``` r
min_channel <- min_stable_channel(channel, flow = 200, resistance = "manning")
plot(demo_bankline)
plot(st_geometry(min_channel), add = TRUE)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

### Hydrographs

Make a hydrograph from scratch:

``` r
hg <- hydrograph(
  2 ~ 0, 3 ~ 1, 5 ~ 2, 3 ~ 5, 2 ~ 10
)
plot(hg)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

Or, from a data frame; this time, also specify a time multiplier:

``` r
data.frame(times = c(0, 1 / 3, 1), flow = c(100, 500, 200)) |> 
  as_hydrograph(times_from = times, flows_from = flow, unit = 24) |> 
  plot()
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

There are canned hydrographs for snowmelt-related and rainfall-related
events, too:

``` r
hyd_rain(peak = 200, baseflow = 50) |> 
  plot()
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

``` r
hyd_snow(peak = 200, baseflow = 50) |> 
  plot()
```

<img src="man/figures/README-unnamed-chunk-8-2.png" width="100%" />

You can easily discretize the hydrograph if you’d like to do manual
calculations:

``` r
discretize_hydrograph(hg, niter = 10)
#> # A tibble: 10 × 2
#>     time  flow
#>    <dbl> <dbl>
#>  1  0     2   
#>  2  1.11  3.22
#>  3  2.22  4.85
#>  4  3.33  4.11
#>  5  4.44  3.37
#>  6  5.56  2.89
#>  7  6.67  2.67
#>  8  7.78  2.44
#>  9  8.89  2.22
#> 10 10     2
```

### Gravel-bed river bank erosion model

Let a hydrograph erode a channel using `gbem()`. That returns a gbem
object encoding information about channel changes. Run `erode()` to
execute the changes to get a new cross section:

``` r
q <- eroding_flow(channel, resistance = "manning")
h <- hyd_rain(median(q) * 4, min(q) / 4)
g <- gbem(h, channel, niter = 1000, resistance = "manning")
```

Calculate the newly eroded channel, and visualize its cross sections:

``` r
new_channel <- erode(g)
plot(demo_bankline)
plot(st_geometry(new_channel), add = TRUE)
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />

## Code of Conduct

Please note that the gbem project is released with a [Contributor Code
of Conduct](https://stochagbem.github.io/gbem/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

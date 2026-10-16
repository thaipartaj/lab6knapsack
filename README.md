# lab6knapsack

<!-- badges: start -->
[![R-CMD-check](https://github.com/thaipartaj/lab6knapsack/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/thaipartaj/lab6knapsack/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

An R package developed for Lab 6 in Advanced R Programming (732A94) at Linköping University.

The package solves the 0-1 knapsack problem: select objects with the highest total value without exceeding a weight limit. Each object can be selected at most once.

It includes brute-force search, dynamic programming, a greedy approximation, and parallel brute-force search.

## Installation

Install the development version from GitHub, including the vignette:

```r
# Install remotes if needed.
install.packages("remotes")

remotes::install_github(
  "thaipartaj/lab6knapsack",
  dependencies = TRUE,
  build_vignettes = TRUE
)
```

Building the vignette runs repeated benchmarks and may take a few minutes.

## Example

Create a data frame with weights `w` and values `v`, then specify the maximum weight `W`:

```r
library(lab6knapsack)

items <- data.frame(
  w = c(10, 20, 30),
  v = c(60, 100, 120)
)

brute_force_knapsack(items, W = 50)
# $value
# [1] 220
#
# $elements
# [1] 2 3
```

The selected objects are rows 2 and 3, with total weight 50 and total value 220.

The other algorithms use the same interface:

```r
# Exact solution; requires integer weights and capacity.
knapsack_dynamic(items, W = 50)

# Fast approximation; may return a lower value.
greedy_knapsack(items, W = 50)

# Exact brute-force search using two workers.
brute_force_knapsack(items, W = 50, parallel = TRUE, cores = 2)
```

## Vignette

The vignette demonstrates the algorithms and compares their performance using repeated measurements:

```r
browseVignettes("lab6knapsack")
```

## Authors

Thai Pham and Hampus Beijer.

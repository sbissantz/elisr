
# elisr – Exploratory Likert Scaling in R <img src='man/figures/elisr.png' align="right" height="139" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/sbissantz/elisr/workflows/R-CMD-check/badge.svg)](https://github.com/sbissantz/elisr/actions)
[![Codecov test coverage](https://codecov.io/gh/sbissantz/elisr/branch/master/graph/badge.svg)](https://codecov.io/gh/sbissantz/elisr?branch=master)
[![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Last-changedate](https://img.shields.io/badge/last%20change-2021--04--10-yellowgreen.svg)](/commits/master)
[![License](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

### Description

An alternative to Exploratory Factor Analysis (EFA) for metrical data in R. The
Exploratory Likert Scaling approach supports the user exploring multidimensional
data structures. In common research practice one usually draws on EFA. As a side
effect researchers often accept overly rigid model assumptions. By dissolving
conjectures like orthogonal factors, the package provides a platform to improve
the identification of relevant dimensions and allows scales to naturally
correlate with each other.

### Welcome `disjoint()` & `overlap()`

`elisr` basically consists of two user functions `disjoint()` and `overlap()`.
With a typical case in mind, the practical difference between them is: (1)
`disjoint()` is set up to produce sharp and disjoint scale fragments. Sharp and
disjoint are those fragments, that include items which (a) share a strong linear
relationship with one another but where (b) any of them is tied to a single
fragment. That is where (2) `overlap()` comes into play. Passing fragments to
`overlap()`, the functions underlying algorithm tries to enrich each fragment.
The emerging scales are flavored with items from your specified data frame, but
the algorithm excludes the ones that are already built into anyone of them.
Making a long story short: Using `overlap()` an item can appear in more than one
of the enriched fragments. In doing so, we overcome the splitting effect of the
data frame induced by `disjoint()`.

### Install from GitHub (development version)

To install the development version, paste the following snippet into your R
console. Note that the first line of code installs the ``devtools`` package (if
not available). The second line then uses ``devtools``' ``install_github()`` to
install ``elisr``.

```r
if (!require(devtools)) install.packages("devtools") 
devtools::install_github("sbissantz/elisr")
```

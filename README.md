# elisr – Exploratory Likert Scaling in R

An Alternative to Exploratory Factor Analysis

<!-- badges: start -->
[![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Travis build status](https://travis-ci.com/sbissantz/elisr.svg?branch=master)](https://travis-ci.com/sbissantz/elisr)
[![R-CMD-check](https://github.com/sbissantz/elisr/workflows/R-CMD-check/badge.svg)](https://github.com/sbissantz/elisr/actions)
<!-- badges: end -->

## Description

``elisr`` implements the Exploratory Likert Scaling approach for metrical data
in R. It thus supports the user when exploring multidimensional data
structures. In common research practice one usually falls back on Exploratory
Factor Analysis. As a side effect researchers often accept rigid model
assumptions. By dissolving conjectures like orthogonal factors, ``elisr``
provides a platform to improve the identification of relevant dimensions and
allows scales to naturally correlate with each other.
 
## `disjoint()` &  `overlap()`

`elisr` basically consists of two user functions `disjoint()` and `overlap()`.
With a typical case in mind, the practical difference between them is: (1)
`disjoint()` is set up to produce sharp and disjoint scale fragments. Sharp and
disjoint are those fragments, that include items which (a) share a strong linear
relationship with one another but where (b) any of them is tied to a single
fragment. That's where (2) `overlap()` comes into play. Passing fragments to
`overlap()` the functions underlying algorithm tries to enrich each fragment.
The emerging scales are fortified with items from your specified data frame but
those in the given fragment. Making a long story short: Using `overlap()` an
item can appear in more than one of the enriched fragments. In doing so, we
overcome the splitting effect of the data frame induced by `disjoint()`. For 

### Install from GitHub (development version)

To install the development version, paste the following snippet into your R
console. Note that the first line of code installs Hadley Wickham, Jim Hester
and Winston Chang's ``devtools`` package (if not available). The second line
then uses ``devtools``' ``install_github()`` function to install ``elisr``.

```r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("sbissantz/elisr")
```

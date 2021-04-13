
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

### The two workhorses: `disjoint()` & `overlap()`

`elisr` comes with two user functions: `disjoint()` and `overlap()`. 

- `disjoint()` "disjointedly disjoints" your list of variables.
- `overlap()` processes `disjoint()`'s result through overlapping.

With a typical case in mind, the practical difference between them is: (1)
`disjoint()` is set up to produce sharp and disjoint scale fragments. Sharp and
disjoint are those fragments, that include items which (a) share a strong linear
relationship with one another but where (b) any of them is tied to a single
fragment. That is where (2) `overlap()` comes into play. Passing fragments to
`overlap()`, the functions underlying algorithm tries to enrich each fragment.
The emerging scales are flavored with items from your specified data frame, but
the algorithm excludes those that are already built into a fragment. Making a
long story short: Using `overlap()` an item can appear in more than one of the
enriched fragments. In doing so, we overcome the splitting effect induced by
`disjoint()`. [...] 

> **Note**: The last section is part of our vignette. If you are interested, you
might read on there.

### Install from GitHub (development version)

There are multiple ways to get `elisr`. I'll show you three, sorted by different
levels of R expertise (pro, skilled or novice). If you don't understand a given
installation instruction -- read on. 

#### Pro

If you are an advanced R user simply download `elisr` from github (e.g., with
`devtools`).

#### Skilled

I stick with `devtools` and `install_github()` to install `elisr`, but feel free
to use whatever you like.

```r
devtools::install_github(sbissantz/elisr)
```

#### Novice

To install the development version, copy and paste the following snippet into
your R console. You will be guided through the installation process. What the
snippet does, is (1) to check if the R package `devtools` is available on your
system and if not (2) asks if you want to install it. If so, (3) it installs
`elisr` via `devtools`' function `install_github()`. After the installation you
need to load and attach `elisr`. Simply type `library(elisr)` and `elisr` will
welcome you.

```r
if (!requireNamespace("devtools", quietly = TRUE)) {
  msg <- "devtools is not installed, want to install it? Type 'yes' or 'no'."
  answer <- readline(prompt = message(msg))
  switch(answer,
         yes = {
           install.packages("devtools")
           devtools::install_github("sbissantz/elisr")
         },
         no = {
           stop("devtools is required to proceed the installation of elisr.",
                 call. = FALSE)
         },
         stop("Please answer 'yes' or 'no'.")
  )
} else {
  devtools::install_github("sbissantz/elisr")
}
```

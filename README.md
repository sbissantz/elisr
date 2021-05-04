
# elisr – Exploratory Likert Scaling in R <img src='man/figures/elisr.png' align="right" height="139" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/sbissantz/elisr/workflows/R-CMD-check/badge.svg)](https://github.com/sbissantz/elisr/actions)
[![Codecov test coverage](https://codecov.io/gh/sbissantz/elisr/branch/master/graph/badge.svg)](https://codecov.io/gh/sbissantz/elisr?branch=master)
[![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

### Description

An alternative to Exploratory Factor Analysis (EFA) for metrical data in R.
Drawing on characteristics of classical test theory, Exploratory Likert Scaling
(ELiS) supports the user exploring multiple one-dimensional data structures. In
common research practice, however, EFA remains as go-to method to uncover the
(underlying) structure of a data set. Orthogonal dimensions and the potential of
overextraction are often accepted as side effects. ELiS confronts these
problems. As a result, elisr provides the platform to fully exploit the
exploratory potential of the multiple scaling approach itself.

### The two workhorses: `disjoint()` & `overlap()`

`elisr` comes with two user functions: `disjoint()` and `overlap()`. 

- `disjoint()` searches highly consistent scale fragments in the haystack of
variables.
- `overlap()` enhances `disjoint()`'s fragments -- selecting items so that they
can overlap across fragments.

With a typical case in mind, the practical difference between them is:
`disjoint()` is set up to produce sharp and disjoint scale fragments. Sharp and
disjoint fragments feature a high internal consistency. Thus, items within such
a fragment share a strong linear relationship with each another. The thing with
`disjoint()` is, it allocates any item to a  particular fragment. This is
where`overlap()` steps in. Passing fragments to `overlap()`, the function's
underlying algorithm tries to enrich each fragment. The emerging scales are
flavored with items from your specified data frame, but the algorithm ignores
those that are already built into a fragment (step 1). Later on, we will talk
about the inclusion criterion in greater detail. To get to the point: Using
`overlap()` an item can appear in more than one of the enriched fragments. In
doing so, we overcome the splitting effect induced by `disjoint()`. These basic
principles will unfold one step at a time in the companion.

> **Note**: The last section is part of `elisr`'s vignette. If you are
interested, you can read on there.

### Install from GitHub (development version)

There are multiple ways to get `elisr`. I'll show you three, sorted by different
levels of R expertise (pro, skilled and novice). If you don't understand a given
installation instruction -- move on to the next. 

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
need to load and attach `elisr`. Simply type `library(elisr)` and `elisr` warmly
welcomes you.

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

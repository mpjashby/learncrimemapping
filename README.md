
# learncrimemapping

<!-- badges: start -->
<!-- badges: end -->

The learncrimemapping package provides various utilities for people using the 
book [_Learn Crime Mapping with R_](https://books.lesscrime.info/learncrimemapping)

## Installation

This package is not on CRAN so you should install the GitHub version using the remotes package:

``` r
# install.packages("remotes")
remotes::install_github("mpjashby/learncrimemapping")
```


## Usage

Installing this package will force the installation of all the packages required to run the code in _Learn Crime Mapping with R_. After setting up R and Positron (on desktop or via Posit Cloud) by following the instructions in [Install the software needed for this book](https://books.lesscrime.info/learncrimemapping/2026/setup.html) and [Chapter 1](https://books.lesscrime.info/learncrimemapping/2026/01_getting_started/) you can check that everything is set up properly by running this code in the R Console:

```r
learncrimemapping::check_workspace()
```

This checks R, the course dependencies listed in this package's DESCRIPTION (including their stated minimum versions), the active Positron workspace where its supported API is available, R's working folder, `here::here()`, each required directory, Air workspace files and R-specific formatting settings, and whether a CARTO API key is present. It prints every success as well as every problem, and explains suggested repairs. 

The package also provides the function `check_code()`, which students can use to run 
various checks on an R script or Quarto file before submitting it for
assessment.

```r
# Choose a file interactively, or supply its path:
check_code("assessment.qmd")
```

`check_code()` runs code in a separate R session, stops on the first error, and stops execution after ten minutes by default. 

For R Markdown and Quarto documents, enabled R chunks are extracted and checked. Chunks explicitly marked `eval = FALSE` or `#| eval: false` are excluded from execution and static checks. Inline R, child documents, dynamic chunk options, and full rendering behaviour are not checked. Run Render in Positron to check the complete document as well.

Static checks use a course configuration, including complexity checking, and ignore local lintr configuration. Feedback distinguishes likely correctness problems, missing packages, and style suggestions. Long string literals (e.g. URLs) are excluded from the 80-character line limit so URLs and paths need not be broken up. These checks suggest improvements; they cannot establish that an analysis is correct.

Passing the result produced by `check_code()` to `write_code_feedback()` produces an HTML report showing the check results.

## Package checks

Run `devtools::check()` before releasing changes. The note reporting packages in
`Imports` that are not used in R code is expected: this metapackage declares the
course dependencies so that installing it also installs those packages. Keep
these dependencies in `Imports`; investigate other dependency notes and any
changes to the expected list.

The installation scripts bundle the checker sources. To check their shell
portability, install `checkbashisms` and ensure it is on `PATH` before starting R
(for example, `brew install checkbashisms` on macOS or install `devscripts` on
Debian/Ubuntu). A missing `checkbashisms` warning means this check was incomplete
and should be resolved in the checking environment.


# learncrimemapping

<!-- badges: start -->
<!-- badges: end -->

The learncrimemapping package provides various utilities for people using the 
book [_Learn Crime Mapping with R_](https://books.lesscrime.info/learncrimemapping)

## Installation

The package requires R 4.3.0 or later so that the current course dependencies
can be installed.

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
check_code()
check_code("assessment.qmd")
```

In Positron, `check_code()` opens an operating-system file dialog. Cancelling
returns quietly without running checks. Linux requires `yad` or `zenity` for
the dialog. If no dialog appears, supply the file path explicitly as above.
Noninteractive calls must supply a file path.

`check_code()` runs code in a separate R session, stops on the first error, and stops execution after ten minutes by default. 

For R Markdown and Quarto documents, enabled R chunks are extracted and checked. Chunks explicitly marked `eval = FALSE` or `#| eval: false` are excluded from execution and static checks. Inline R, child documents, dynamic chunk options, and full rendering behaviour are not checked. Run Render in Positron to check the complete document as well.

Static checks use a course configuration, including complexity checking, and ignore local lintr configuration. Feedback distinguishes likely correctness problems, missing packages, and style suggestions. Long string literals (e.g. URLs) are excluded from the 80-character line limit so URLs and paths need not be broken up. These checks suggest improvements; they cannot establish that an analysis is correct.

Passing the result produced by `check_code()` to `write_code_feedback()` produces an HTML report showing the check results.

## Package checks

GitHub Actions runs full package checks and installation smoke tests on Linux,
macOS and Windows with release R, and on Linux with R 4.3, the previous release
and R-devel. The installation smoke test checks a fresh package library and
the bundled container resources. Separate workflows check Docker integration,
workflow and shell validity, and generated documentation. Checks run on pushes,
pull requests and weekly schedules, and can also be started manually.

To run the package checks locally, install the package dependencies,
`rcmdcheck` and `pkgbuild`, then run
`Rscript .github/scripts/check-package.R` from the repository root. The output
is saved in the ignored `check/` directory. Checks allow the recorded
metapackage unused-Imports note and fail on errors, warnings or other notes.

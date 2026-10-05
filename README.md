
# learncrimemapping

<!-- badges: start -->
<!-- badges: end -->

The learncrimemapping package provides various utilities for people using the 
book [_Learn Crime Mapping with R_](https://books.lesscrime.info/learncrimemapping)

## Installation

You can install the development version of learncrimemapping like so:

``` r
# install.packages("remotes")
remotes::install_github("mpjashby/learncrimemapping")
```

## Usage

The main function of this package is to install all the R packages a user will
need to run the code included in _Learn Crime Mapping with R_.

Students can check their setup from Positron's R Console:

```r
learncrimemapping::check_workspace()
```

This read-only report checks R, the course dependencies listed in this
package's DESCRIPTION (including their stated minimum versions), the active
Positron workspace where its supported API is available, R's working folder,
`here::here()`, each required directory, Air workspace files and R-specific
formatting settings, and CARTO key presence. It prints every success as well as
every problem, explains suggested repairs, and never prints API keys.
No `.Rproj` file is required. It does not install packages, change the working
folder, reset here, activate a usethis project, edit settings or write files.

```r
results <- learncrimemapping::check_workspace()
results$counts
results$checks[["workspace.here"]]

# Inspect a folder independently of which folder Positron has open:
learncrimemapping::check_workspace("~/Documents/crime_mapping")

# Optionally check the published R release, with a bounded network lookup:
learncrimemapping::check_workspace(check_updates = TRUE)
```

The default needs no network connection. Failed release lookups require manual
verification and are not setup failures. Positron version and updates, inherited
editor settings and actual Air formatting behaviour require manual checks.
The report gives those instructions and never announces complete setup while
checks remain unverified. Windows checks use pkgbuild's Rtools detection in an
isolated process; other systems are not assumed to have compilation tools.

`results$checks` contains stable named entries with `id`, `status`, `message`,
`details` and `actions`. Statuses are `passed`, `problem`, `manual`,
`not_checked` and `not_applicable`. `results$counts` counts each status.
CARTO results contain only presence and startup-file selection, never key
values. Normal R startup-file selection is checked for the next start;
custom startup code, startup flags and a previous working folder cannot be
reconstructed. Restart R in Positron and repeat the check to verify presence.

The implementation follows the local 2026 textbook sources and official
[Positron session detection guidance](https://positron.posit.co/migrate-rstudio-settings-and-extensions.html),
[Positron workspace API guidance](https://positron.posit.co/guide-r-session-hooks.html),
[Air settings guidance](https://posit-dev.github.io/air/editor-vscode.html),
[editor setting precedence](https://code.visualstudio.com/docs/configure/settings),
and [R startup rules](https://stat.ethz.ch/R-manual/R-devel/library/base/html/Startup.html).

It also provides the function `check_code()`, which students can use to run 
various checks on an R script or Quarto file before submitting it for
assessment.

```r
# Choose a file interactively, or supply its path:
check_code("assessment.qmd")

# Override the execution directory when your data are elsewhere:
results <- check_code("scripts/assessment.R", execution_dir = "coursework")

# Run only static checks:
results <- check_code("assessment.R", reprex = FALSE)
```

`check_code()` requires R 4.1 or later and lintr 3.4.0 or later. It runs code
in a separate R session using callr and evaluate, stops on the first error,
and stops execution after ten minutes by default. The `reprex` argument keeps
its original name for compatibility, but reprex is no longer the execution
engine. Execution uses the current package library paths without loading
personal or project startup profiles or copying workspace objects.

Unless `execution_dir` is supplied, execution uses the nearest RStudio or
Quarto project root above the submitted file. Outside a project, it uses the
file's directory. The chosen directory is printed before execution. A different
limit can be supplied with `timeout` (in seconds).

For R Markdown and Quarto documents, enabled R chunks are extracted without
writing a companion script. Chunks explicitly marked `eval = FALSE` or
`#| eval: false` are excluded from execution and static checks. Inline R,
child documents, dynamic chunk options, and full rendering behaviour are not
checked. Run Render in RStudio to check the complete document as well.

Static checks use a course configuration, including complexity checking, and
ignore local lintr configuration. Feedback distinguishes likely correctness
problems, missing packages, and style suggestions. Long string literals are
excluded from the 80-character line limit so URLs and paths need not be broken
up. These checks suggest improvements; they cannot establish that an analysis
is correct.

The function invisibly returns a list containing `file`, `execution_dir`,
`execution`, `lints`, `failures`, `report`, and `versions`. Execution status is
`skipped`, `success`, `error`, `timeout`, or `failed` (a checker failure).
`execution$events` retains captured code, output, messages, warnings, errors,
and plot paths. Runtime locations indicate the start of the executing
expression; static findings include original line and column numbers.
Console feedback shows up to three warnings; the returned results and full
report retain every captured warning, including warnings preceding an error.

The full text report and any plots remain in temporary storage for the current
R session. Copy them elsewhere if you need to retain them. Static checks still
run after execution fails or times out. Execution isolation protects your R
workspace, but submitted code can still read and write files and use the network.

## Development checks

Regression tests can run independently of the other course packages:

```r
source("R/check_code.R")
source("R/check_workspace.R")
testthat::test_dir("tests/testthat")
```

The checker compatibility workflow runs on Windows, macOS, and Linux with both
current and minimum supported versions of callr, evaluate, and lintr. The
minimum-dependency Linux job also uses R 4.1. Normal package checks run the same
tests through `tests/testthat.R`.

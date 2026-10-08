# Manual feedback examples

Run the commands below from the learncrimemapping project directory. These are
synthetic submissions, with small local datasets and no downloads or package
installation calls. Some deliberately contain invalid R or code that fails.
They are excluded from package builds and are not part of the automatic test
suite. Do not source a submission to check it: use the checker from the Console.

Load the current project version using RStudio's **Load All**, or:

```r
devtools::load_all(".")
```

## Check one file and create an HTML report

```r
examples_dir <- "tests/manual-feedback"
result <- learncrimemapping::check_code(
  file.path(examples_dir, "r", "style_issues.R"),
  timeout = 10
)

result$issues
result$execution$status

report <- tempfile(fileext = ".html")
write_code_feedback(result, report, participant = "Example")
browseURL(report)
```

Change the path to try any example below. For Quarto files, the checker extracts
enabled R chunks; it does not render the document. Syntax and runtime error
examples deliberately cannot render successfully.

| File | Expected behaviour |
| --- | --- |
| `r/clean_example.R` | Runs and captures a simple plot, with no issues. |
| `quarto/clean_example.qmd` | Same clean code, with document prose and original document line numbers. |
| `r/style_issues.R`, `quarto/style_issues.qmd` | Comment spacing/case, three naming occurrences collapsed into two distinct names, quotes, assignment, comma spacing and indentation feedback. Runs successfully. |
| `r/runtime_error.R`, `quarto/runtime_error.qmd` | Intentional runtime error; earlier expressions run and the later assignment is not executed. Report identifies the failing line/block. |
| `r/syntax_error.R`, `quarto/syntax_error.qmd` | Missing parenthesis. No code is executed; static feedback may still be available. |
| `r/conditions.R` | One non-routine message and one ordinary warning attract feedback. Synthetic loading/masking messages and the longitude/latitude surface warning remain only in the captured log. |
| `quarto/conditions.qmd` | All three messages and both warnings attract feedback. The conditions are synthetic, so package versions cannot change this comparison. |
| `r/inspection_calls.R` | Three standalone inspection calls are flagged. Assigned results and the intermediate pipe stage are exempt. |
| `r/embedded_checker.R`, `quarto/embedded_checker.qmd` | Embedded diagnostic call blocks all execution, with the console-only explanation and a failed initial runtime-error check. |
| `r/crs_area.R` | Requires installed `sf`: EPSG:27700 passes for London; EPSG:26967 attracts an area-of-use finding. No external data needed. |
| `r/timeout_example.R` | With `timeout = 1`, execution is interrupted during a three-second sleep. With the default or ten-second limit, it completes. |
| `r/Bad.File Name.R` | Clean code with an intentionally unsuitable filename. Only the filename should attract feedback. |
| `quarto/disabled_chunk.qmd` | Disabled code contains invalid syntax and bad style, but should be excluded. Enabled code runs and produces a plot. |

To exercise timeout feedback:

```r
result <- learncrimemapping::check_code(
  file.path(examples_dir, "r", "timeout_example.R"), timeout = 1
)
```

## Check a Moodle-shaped batch

The helper defines a function; sourcing it does not run the checks. It packages
all 17 submissions, keeping their original filenames, in Moodle-style folders
using synthetic participant IDs. It needs `zip` and the R package `withr`.

```r
source(file.path(examples_dir, "make_batch.R"))
archive <- make_manual_feedback_zip(examples_dir)
output_dir <- tempfile("manual-feedback-reports-")

batch <- check_submissions(
  archive,
  output_dir = output_dir,
  timeout = 10,
  profile = list(name = "Manual checker examples")
)

batch$manifest
browseURL(batch$index)
```

Use a new or empty output directory each time. This mixed R/Quarto archive does
not set an expected extension. Set `timeout = 1` to include timeout cases, but
some other files may then reach the limit during package startup as well.

You can edit or duplicate the files to explore other cases. Changing a fixture
filename can introduce filename-style feedback in addition to the intended case.

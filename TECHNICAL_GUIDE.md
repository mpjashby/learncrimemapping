# Automated submission feedback reference

This reference explains exercise profiles, feedback rules and the limits of the
automated checker. For package installation, Docker setup, running or resuming a
batch and uploading reviewed reports to Moodle, follow [MARKING.md](MARKING.md).
The reports provide feedback, not grades; analytical and visual correctness
require assessor review.

`check_code()` and `check_submissions()` use one quiet backend and the same
feedback interpretation. Console, text and HTML reports are presentations of
the structured `issues` list. With style checking enabled, staff batches also
add advisory checks for chains of single-use intermediate objects and missing
blank lines before section comments beside multiline statements. These checks
require manual judgement and are not run by student-facing `check_code()`.

## Matching student checks

Share the exercise profile used for the staff batch with students so both use
the same exercise requirements:

```r
result <- learncrimemapping::check_code(
  "scripts/exercise_01.R",
  profile = list(name = "Week 1 exercise", expected_extension = "R")
)

result$issues
```

The available profile settings are `name`, `expected_extension`,
`required_declaration` and `text_output`. A required declaration is a literal
string that must appear in an R comment; it does not assess the truth of an AI
declaration. `text_output = "allow"` permits expected text and message output.
Unknown profile settings are rejected to avoid silently ignoring a typo.

Runtime errors and warnings, existing course linters, explicit installation
calls, and optional exercise requirements produce issues with stable identifiers,
categories, locations, evidence, explanations and suggested actions. Repeated
HTML findings are grouped while retaining all locations. Script text output and
unrecognised messages receive cautious review prompts. Routine package/import
notices and download receipts are retained in the log without unnecessary-output
feedback for R scripts. Quarto messages remain review prompts, including routine
package messages, because they should not appear as raw R output in the rendered
document. Quarto text output is allowed because it can be intended document
content. Plots are embedded in HTML reports; HTML does not load remote scripts
or styles, and student text is escaped.

Reports identify the course as SECU0005 Crime Mapping and show initial results
for syntax errors, runtime errors and warnings, environment problems, timeouts
and checker failures. Positive statements use a check mark when no problem was
detected and a red cross when a problem was found. A check that did not run or
finish is labelled accordingly. The submitted code appears with original line numbers
and static R syntax highlighting before the visualisations. New check results
retain a source snapshot, so the report does not change if the original file is
edited afterwards. Invalid R is still shown safely as plain code.

## Execution environment and limits

Each batch submission runs in a fresh Docker container with a separate R session
and `here()` root,
`data/raw`, `data/processed`, `outputs` and `scripts` directories. Supply
`workspace_template` to copy exercise resources into each workspace separately.
Templates cannot contain startup profiles, saved R workspaces, symbolic links or
the reserved `.sandbox` directory. Build the image first; see [MARKING.md](MARKING.md).
No student code is rewritten, and student submissions cannot share these
execution directories by accident.

Extracted submissions and execution workspaces, including private package
libraries, downloads and caches, are temporary. They are removed after each
self-contained HTML report is saved. Reports embed source code, plots and
captured logs; `results.rds` retains structured results, but paths to temporary
files are historical references. Keep the original Moodle ZIP for reviewing
submitted files. The persistent batch outputs and resumption procedure are
described in [MARKING.md](MARKING.md).

By default, `build_checker_image()` preinstalls the packages listed in
`DESCRIPTION` Imports except `gifski`, plus their required R dependencies and
the core checker packages. Optional `Suggests` are not installed. Package
versions are those available on CRAN when the image is built; subsequent batches
reuse those versions until it is rebuilt. Customise the preinstalled R and system
packages with the build function's `packages` and `system_packages` arguments;
see [MARKING.md](MARKING.md) for setup and rebuilding instructions.
The image does not include RStudio Server, TeX, Quarto or Pandoc; documents have
their enabled R chunks checked without rendering. In Docker batches, explicit
and implicit package installations are allowed and use each student's private
library beneath their workspace. The preinstalled and host libraries cannot be changed. The
local backend and `check_code()` retain the preflight that blocks recognised
explicit installation/update calls. That preflight is not a security boundary.

Course package-loading feedback requires one `pacman::p_load()` call or
namespace-qualified calls with `::`. `library()`, `require()` and additional
`p_load()` calls receive an instruction to consolidate package loading.
Submitted ZIP files are submission errors, receive an explicit message that
they could not be checked, and are never displayed as source code or unpacked
to select a file automatically.

Run `check_code()` from the console. Including it in submitted R code or an
enabled document chunk receives specific feedback and prevents execution, so
interactive file prompts and recursive checking cannot obscure the real issue.
Syntax errors likewise prevent all execution; the report gives the parser's
message and original-file line. Syntax and runtime errors appear before warnings
under “Potential problems”. Long URL/string contents are exempt
from line-length checks, while genuinely long comments and expressions remain
subject to the 80-character rule.

The backend does not render documents or reproduce chunk display options, so
students should check their rendered output, including any `message: false`
settings they already use. The longitude/latitude `st_point_on_surface` warning is retained in the log but
suppressed in R-script feedback. Quarto retains this warning; point-geometry
conditions explain that point coordinates are unchanged.

Executed `sf::st_transform()` calls are observed in the child session and the
target's EPSG area-of-use bounds are obtained from sf's local PROJ database.
The input geometry is located in longitude/latitude using its source CRS, and
all geometry vertices are compared with the bounds, including extents crossing
the antimeridian. Data entirely outside the area and data extending beyond it
receive feedback with the CRS name/code and the compared extents. EPSG bounds
are approximate rectangles: an inside result does not prove that a CRS is
appropriate or that all geometry between its vertices lies inside the area.
The check assumes the source CRS was assigned correctly. Missing metadata,
unlocatable geometry, non-EPSG CRS definitions and custom transformation
pipelines receive an incomplete-check explanation rather than a pass.
Empty geometry placeholders are logged as not checked without generating an
issue. Repeated checks during plotting are consolidated per target, outcome and
executing expression, with all observations retained in the log.
Transformations through other packages and code not reached during execution
are outside this check. `reprex = FALSE` does not perform CRS area checks.
sf, GDAL and PROJ versions are recorded alongside checker versions.

Timeout feedback identifies the configured limit and last expression. For tile
requests it asks students/staff to inspect extent, zoom, network and service
availability without attributing every timeout to a student mistake. In the local backend, explicit
package-installation calls are labelled as blocked execution rather than a
failure of the checker. Container batches permit installation into private libraries. No exercise-specific output/data requirements are added.

Docker batch checking mounts only the current student's workspace and uses a
read-only system filesystem. Student package installations go into their private
workspace library. Memory, CPU, process-count and timeout limits are enforced;
workspace disk use and network destinations are not restricted. Containers are
removed after each run, including timeout/failure. The local backend and
`check_code()` have filesystem and network access without these restrictions.
Recognisable network/tile failures receive environment feedback and flag an
incomplete check rather than being automatically attributed to the student.

The shared policy is recorded alongside dependency versions. Use the same
exercise profile, resources and package versions for comparable student and
staff results. Execution stops at the first error; style checks still run after
errors and timeouts. Runtime locations identify the start of an expression.

For `.qmd` and `.Rmd`, only enabled R chunks are checked, preserving original
line numbers. Full rendering, inline expressions, child documents and dynamic
document/chunk settings remain outside the current checker. These limitations
appear in each document report. Analytical and visual correctness still need
manual review.

## Development checks

```r
source("R/check_code.R")
source("R/code_feedback.R")
source("R/check_submissions.R")
source("R/write_code_feedback.R")
source("R/spatial_feedback.R")
source("R/check_workspace.R")
testthat::test_dir("tests/testthat")
```


Standalone inspection calls (`view()`, `View()`, `excel_sheets()`, `head()`,
`tail()`, `slice()`, `slice_head()`, `slice_tail()` and `glimpse()`) receive a
style suggestion to run temporary inspection in the console. Calls whose results
are assigned or consumed by another function are exempt, including intermediate
pipe stages. A terminal inspection stage in an unassigned pipe is flagged.
Missing-argument lints include trailing-comma guidance and the enclosing call.

Issue counts include suggestions and review prompts; they are not grades.
The assessor index separates execution outcomes from feedback status. Status
prioritises errors, warnings, messages and remaining style/review issues. EPSG
area-of-use findings count as warnings. Unsupported submissions and incomplete
execution are skipped; syntax errors and embedded checker calls are errors.
Suppressed R conditions do not influence the status or initial checks. Full
captured logs remain available, including suppressed conditions.


Submitted file names are checked as part of the shared style checks, using only
the basename (not the directory path). The stem must contain lower-case ASCII
letters or digits, with single underscores between words; numeric prefixes are
allowed. Spaces, extra periods, capitals and other punctuation are flagged.
The period before the supported file extension is exempt, and `.R` is allowed.
Files are checked under their original names and are never renamed automatically.


Comment checks follow the Tidyverse requirement for one space after `#`
(https://style.tidyverse.org/syntax.html#comments). Sentence case is a course
requirement checked conservatively: lower-case prose starts, clear title case
and all-capital prose are review suggestions. Wrapped continuation lines,
identifiers, acronyms, separators, shebangs, roxygen and Quarto directives are
handled separately. Natural-language case cannot be verified exhaustively;
manual review is still necessary. Parsed comments before syntax errors are
checked, but strings, document prose and disabled Quarto chunks are excluded.

Object-name lints list all distinct problematic names in one note, with all
source locations. Naming uses the snake-case linter without the symbols style.
Repeated identical rules (including quote and spacing notes) remain grouped.
Indentation and line-length variants now share one note per rule, retaining the
individual diagnoses in collapsed extracts and the raw lint results. The console
summary also collapses these layout notes and repeated course comments.

Initial checks count an embedded `check_code()` call as a runtime error, even
though preflight blocks execution. Incomplete runtime/environment checks use
“Full check not possible – check for errors below”.


Error descriptions state the limits of execution checking. Syntax errors and
blocked diagnostic calls prevent all execution checks. Runtime errors identify
the start line of the failing expression/block and explain that code after that
block was not checked for runtime issues. Static style checks can still produce
findings, including in code that could not be executed.

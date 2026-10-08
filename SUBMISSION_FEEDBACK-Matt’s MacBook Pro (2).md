# Automated weekly submission feedback

`check_code()` and `check_submissions()` use one quiet backend and the same
feedback interpretation. Console, text and HTML reports are presentations of
the same structured `issues` list. The existing `check_code()` arguments and
return fields remain available; `profile`, `issues` and `scope` are additions.

## A weekly batch

```r
week_01 <- list(
  name = "Week 1 exercise",
  expected_extension = "R"
)

batch <- learncrimemapping::check_submissions(
  zip = "SECU0005_26-27-Upload your code for the Week 1 exercise-9071365.zip",
  output_dir = "feedback/week-01",
  profile = week_01,
  timeout = 600
)

batch$manifest
batch$index
```

Use a new or empty output directory. Each Moodle
`Participant_ID_assignsubmission_file` or `Name_ID_assignsubmission_file`
folder must contain one nonempty `.R`,
`.qmd` or `.Rmd` file. Missing, extra, empty and unsupported files receive
feedback reports. ZIP metadata files are ignored. Unsupported folder layouts,
nested submission paths, duplicate paths, unsafe paths and excessively large
archives are rejected before any code runs. Different folders with the same
participant ID are rejected as ambiguous. Named folders retain their original
paths in the manifest, but reports use `Participant ID` as their identity.

The output directory contains:

- `index.html`: assessor overview linking to each student's report.
- `feedback/Participant_ID.html`: portable reports to distribute individually.
- `manifest.csv`: participant IDs, original paths, checksums, execution statuses,
  issue counts, incomplete-check flags and report paths.
- `results.rds`: all structured results, including captured events and issues.
- `submissions/`: extracted originals.
- `workspaces/`: separate execution directories, generated files and checker logs.

Completed results are checkpointed after each participant. An interrupted batch
retains completed reports, but automatic resumption is not yet implemented.
Issue counts include suggestions and review prompts; they are not grades.

## Matching student checks

Share the exercise profile with students:

```r
result <- learncrimemapping::check_code(
  "scripts/exercise_01.R",
  profile = week_01
)

learncrimemapping::write_code_feedback(result, "my-feedback.html")
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
feedback. Quarto text output is allowed because it can be intended document
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

Each batch submission runs in a separate R session with a fresh `here()` root,
`data/raw`, `data/processed`, `outputs` and `scripts` directories. Supply
`workspace_template` to copy exercise resources into each workspace separately.
Templates cannot contain startup profiles, saved R workspaces or symbolic links.
No student code is rewritten, and student submissions cannot share these
execution directories by accident.

Preinstall course packages. Explicit package installation/update calls prevent
execution and receive feedback, including when style checks are disabled. This
is a conservative preflight, not an exhaustive detection of installation or
other side effects. Package loaders such as `pacman::p_load()` are permitted;
ensure their requested packages are already installed.

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
under “Execution problems and warnings”. Long URL/string contents are exempt
from line-length checks, while genuinely long comments and expressions remain
subject to the 80-character rule.

Routine package-loading and import messages are retained in the captured log
but omitted from feedback for R scripts. Quarto/R Markdown reports retain them
as review prompts: raw R messages should not appear in the rendered output.
The backend does not render documents or reproduce chunk display options, so
students should check the rendered result, including any `message: false`
settings they already use. Point-geometry label warnings receive an explanation
that point coordinates are unchanged; that explanation is not given for polygons.

Executed `sf::st_transform()` calls are observed in the child session and the
target's EPSG area-of-use bounds are obtained from sf's local PROJ database.
The input geometry is located in longitude/latitude using its source CRS, and
all geometry vertices are compared with the bounds, including extents crossing
the antimeridian. Data entirely outside the area and data extending beyond it
receive feedback with the CRS name/code and the compared extents. EPSG bounds
are approximate rectangles: an inside result does not prove that a CRS is
appropriate or that all geometry between its vertices lies inside the area.
The check assumes the source CRS was assigned correctly. Missing metadata,
empty/unlocatable geometry, non-EPSG CRS definitions and custom transformation
pipelines receive an incomplete-check explanation rather than a pass.
Transformations through other packages and code not reached during execution
are outside this check. `reprex = FALSE` does not perform CRS area checks.
sf, GDAL and PROJ versions are recorded alongside checker versions.

Timeout feedback identifies the configured limit and last expression. For tile
requests it asks students/staff to inspect extent, zoom, network and service
availability without attributing every timeout to a student mistake. Explicit
package-installation calls are labelled as blocked execution rather than a
failure of the checker. No exercise-specific output/data requirements are added.

The child process has filesystem and network access: `callr` is not a security
sandbox. Run unfamiliar code in an appropriately restricted environment. This
implementation provides execution timeouts, archive limits and separate
workspaces; it does not enforce memory, network, library-write or total runtime
output limits. External services and package differences can change results.
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

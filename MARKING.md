# Producing Moodle feedback reports

This workflow takes a ZIP downloaded from a Moodle assignment, checks the
submitted code with `check_submissions()`, and produces a
second ZIP containing one HTML feedback report per submission. Moodle matches
each report to a submission using the **original Moodle folder name**, including
its numeric identifier. Preserve that name exactly; the identifier is not a
student number and must not be substituted with one.

The reports provide automated code feedback, not grades. Review them before
distribution and assess analytical and visual correctness separately. See
[SUBMISSION_FEEDBACK.md](SUBMISSION_FEEDBACK.md) for the checks and their limits.

Follow steps 1–5 each time you mark an assignment. Before marking on a computer
for the first time, complete the [one-time Docker setup](#one-time-docker-setup-on-each-computer)
at the end of this guide.

## 1. Download the submissions from Moodle

1. Enable **Download submissions in folders** in the options above the grading
   table.
2. Choose **Download all submissions** from **Grading actions**. Keep the
   downloaded ZIP unchanged in a private marking directory outside this Git
   project. Do not commit submissions, reports or marking records to Git.
3. Inspect the ZIP's file listing. Its top level must contain Moodle submission
   folders, for example:

   ```text
   Participant_12345_assignsubmission_file/exercise_01.R
   Participant_67890_assignsubmission_file/exercise_01.R
   ```

   Named folders such as `Jane Smith_12345_assignsubmission_file` also work.

The checker expects one nonempty `.R`, `.qmd` or `.Rmd` file per folder. Empty,
unsupported or multiple files receive submission-error reports. A ZIP submitted by a student is not unpacked by the checker. Archives exceeding 5,000 entries or 100 MB uncompressed, unsafe paths, and ambiguous participant IDs are rejected.


## 2. Prepare R and the checking environment

Start Docker Desktop and wait until it reports that the engine is running.
Leave it running throughout the batch. Use the checker image already built on
this computer; you do **not** need to run `docker build` for each assignment.
If this is the first run on this computer, or the image has been deleted,
complete the [one-time setup](#one-time-docker-setup-on-each-computer) first.

Each student's code runs in a fresh container with its own workspace. Downloads
remain enabled, and additional packages can install into the student's private
library without changing your computer's package libraries. Docker is required
for static-only batches too; an unavailable engine or image stops the batch
without falling back to unrestricted local execution.

Open this project in R and load the current local package so the batch checker
documented here is available. Marking functions are internal and are loaded
locally by `devtools::load_all()`:

```r
devtools::load_all()
```

Set the paths for this marking run. Replace the example paths with your own;
use a new results directory for each run:

```r
submission_zip <- normalizePath(
  "~/marking/week-01/moodle-submissions.zip",
  winslash = "/", mustWork = TRUE
)
run_dir <- path.expand("~/marking/week-01/run-01")
upload_dir <- path.expand("~/marking/week-01/moodle-feedback-01")
feedback_zip <- path.expand("~/marking/week-01/moodle-feedback-01.zip")

profile <- list(name = "Week 1 exercise", expected_extension = "R")
```

Change the profile to match the actual exercise (`"qmd"` or `"Rmd"` where
appropriate). Use the same profile shared with students. Optional settings
include `required_declaration` (a literal string expected in an R comment) and
`text_output = "allow"` when text output is intended.

## 3. Generate the reports

```r
batch <- check_submissions(
  zip = submission_zip,
  output_dir = run_dir,
  profile = profile,
  timeout = 600,
  container_memory = "4g",
  container_cpus = 2
)

batch$manifest
utils::browseURL(batch$index)
```

`timeout` is the limit in seconds **per submission**; the batch runs
sequentially. Docker also enforces a memory limit (4 GB by default), CPU limit
(two CPUs) and process-count limit (256). The container monitor allows another
30 seconds for checker setup, static checks and collecting results, then removes
the container and all its processes. Adjust memory/CPU settings for larger
exercises. There is no workspace disk quota; downloads and private package
installs consume space in the results directory. If the exercise needs supplied data, pass `workspace_template`.

The results directory contains:

| Path | Purpose |
| --- | --- |
| `index.html` | Assessor overview with links to individual reports |
| `feedback/Participant_12345.html` | Individual portable HTML feedback report |
| `manifest.csv` | Participant-to-report mapping and checking outcomes |
| `results.rds` | Structured results for further review |
| `submissions/` | Extracted original submissions |
| `workspaces/` | Execution files and checker logs |

Completed results are saved after each participant. If the batch is interrupted,
keep those records, resolve the interruption and rerun into a **new** results
directory. Automatic resumption is not implemented. Container-created symbolic links are
rejected before host report processing. Package only a completed,
reviewed batch using the checks below.


## 4. Put reports into the original Moodle folders and create the ZIP

Run the following after completing the review. It uses the original ZIP to
recover folder names, including empty submission folders, and refuses to
package an incomplete batch or overwrite an earlier upload.

```r
zip_contents <- package_moodle_feedback(
  submission_zip, run_dir, upload_dir, feedback_zip
)
zip_contents
```

Inspect `zip_contents`: there should be exactly one nonempty report per Moodle
folder, with each path ending in `/feedback.html`. Compare the count with the
intended submissions in Moodle. Extract the ZIP into a temporary directory and
open a few reports to verify the packaged copies, including an error report
and a report containing plots where available. Keep the original download and
the full results directory privately for audit and troubleshooting.


## 5. Upload the feedback ZIP to Moodle

1. Choose **Upload multiple feedback files in a zip** from **Grading actions**.
2. Upload the `moodle-feedback-01.zip` produced above, then choose
   **Import feedback file(s)**.

## One-time Docker setup on each computer

Complete this setup before your first marking run on each computer. Once the
image is built, subsequent assignments reuse it: start Docker Desktop and follow
steps 1–5 above.

### Install Docker Desktop and build the image

1. Install [Docker Desktop](https://docs.docker.com/desktop/setup/install/mac-install/).
   On an Apple silicon Mac, choose the Apple silicon installer. If Docker Desktop
   is already installed, keep that installation.
2. Start Docker Desktop and wait until the engine is running.
3. Open Terminal in this repository's root directory and run the code below.
   `Rscript` must be available in Terminal. The build needs internet access and
   free disk space for downloaded files and package compilation.

```sh
course_packages=$(Rscript --vanilla -e '
  imports <- read.dcf("DESCRIPTION")[1, "Imports"]
  packages <- trimws(sub("[(].*[)]", "", strsplit(imports, ",", fixed = TRUE)[[1]]))
  packages <- setdiff(packages, "gifski")
  cat(paste(packages, collapse = ","))
')

docker build --platform linux/arm64 \
  --build-arg "EXERCISE_PACKAGES=$course_packages" \
  --build-arg "EXERCISE_SYSTEM_PACKAGES=git libgit2-dev libjpeg-dev libpng-dev libtiff-dev libwebp-dev libicu-dev libtbb-dev libnode-dev" \
  -f inst/container/Dockerfile -t learncrimemapping-checker:local .
```

The command above targets Apple silicon. On an Intel Mac or AMD64 Linux computer,
replace `--platform linux/arm64` with `--platform linux/amd64`. The pinned
R-only `rocker/r-ver:4.5.3` base supports both platforms; the checker detects the
built image's architecture when starting containers. No emulation is needed.

Wait until the build completes successfully before marking. The first build can
take considerable time because it compiles R packages on ARM. It is smaller than
the previous geospatial image, but still needs working disk space; its full build
and final size have not yet been verified. The build context includes package
source, never submissions or marking records.

### What the image contains

The build preinstalls the packages listed in `DESCRIPTION` Imports, except the
rarely needed `gifski`, plus their required R dependencies. The package names are
read automatically from `DESCRIPTION`; optional `Suggests` are not installed.
Version constraints are removed from the names: packages are installed from the
current CRAN repository rather than pinned to those minimum versions. Each
marking result records the exact image ID for reproducible reruns.

The image also includes R, the checker helpers, GDAL, GEOS, PROJ, UDUNITS, HTTPS
support, font/image libraries, Git bindings, JavaScript libraries and compilation
tools for student package installations. It omits RStudio Server, TeX, Quarto,
Pandoc and Rust/Cargo. `rstudioapi` is an R interface package, not RStudio itself.
Documents have their enabled R chunks checked; they are not rendered.

Preinstalled libraries are read-only. Extra packages installed by student code,
including through `pacman::p_load()`, go into that student's
`/workspace/.sandbox/library`. HOME, temporary files and caches are also inside
their workspace. These files remain in the marking results for inspection and
are not shared with other students. Allow extra execution time for any package
compilation; missing system dependencies or network failures require assessor
review. For an occasional exercise requiring GIF encoding, prepare an image
with Rust/Cargo and `gifski` for that exercise.

The assessor's home, other submissions and host package libraries are not
mounted in a student's container. R, packages and HTTPS certificates inside the
image remain readable. Bridge networking allows web requests and does not
filter internet destinations or prevent access to reachable network services;
only pass credentials required by the exercise. `backend = "local"` selects
unrestricted checking and should be reserved for trusted fixtures.

### When to rebuild

Rebuild after changing the checker code, changing the preinstalled package list
or choosing to update package versions. Repeat setup if Docker's images are
deleted or its stored data is reset. Each new computer needs its own image build
unless you transfer the existing image to it.

New student submissions, exercise resources, profile settings and output paths
do **not** require rebuilding the image.

### Verify the container isolation after setup

After building the image, run the opt-in integration tests from Terminal. This
is a setup check, not a step required for every assignment:

```sh
LCM_TEST_DOCKER=true Rscript -e 'source("R/check_code.R"); source("R/check_workspace.R"); testthat::test_dir("tests/testthat", filter = "submission-container", stop_on_failure = TRUE)'
```

These check host-file isolation, read-only system libraries, private package
installation, separation between students, HTTPS downloads, portable plots and
timeout cleanup. Docker Desktop must stay running. A custom image can be selected
with `container_image` in R and `LCM_TEST_IMAGE` for these tests.

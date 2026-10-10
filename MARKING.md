# Producing Moodle feedback reports

This workflow takes a ZIP downloaded from a Moodle assignment, checks the
submitted code with `check_submissions()`, and produces a
second ZIP containing one HTML feedback report per submission. Moodle matches
each report to a submission using the **original Moodle folder name**, including
its numeric identifier. Preserve that name exactly; the identifier is not a
student number and must not be substituted with one.

The checker also offers suggestions about code style, such as simplifying long
sequences of intermediate objects or adding space before section comments.
These are suggestions for you to review: students may have good reasons for
writing their code that way.

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
   downloaded ZIP unchanged in a private marking folder. Keep student
   submissions, reports and marking records out of public or shared code repositories.
3. Open the ZIP to check its contents, keeping the original ZIP unchanged. Its top level must contain Moodle submission
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

Each student's code runs separately. It can download data and install additional
R packages into its own separate package library, without changing your
computer's R package libraries. If Docker is unavailable, the checker stops.

Open the R project you use for marking. This can be in any folder; no local
copy of the package source files is needed.
The one-time setup below installs the package so you can use it from your
marking project. The three colons in `learncrimemapping:::check_submissions()`
are intentional: this is a staff function, separate from the functions offered
to students. You do not need to run `library(learncrimemapping)` first.

Set the paths for this marking run. Replace the example paths with your own;
use a new results folder for each run. In these examples, `~` means your home
folder. Create a folder such as `marking/week-01` there and save the Moodle ZIP
in it before running this code in the **R console**:

- `submission_zip` is the ZIP you downloaded from Moodle.
- `run_dir` is a new folder where the checker will save its results.
- `upload_dir` is a new folder used to organise the feedback for Moodle.
- `feedback_zip` is the ZIP you will eventually upload to Moodle.

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

The `profile` describes the exercise. Set `name` to its title and
`expected_extension` to `"R"`, `"qmd"` or `"Rmd"`, as appropriate. If students
already use an exercise profile with `check_code()`, use that same profile here.
See [SUBMISSION_FEEDBACK.md](SUBMISSION_FEEDBACK.md) for additional settings.

## 3. Generate the reports

```r
batch <- learncrimemapping:::check_submissions(
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

Run this code in the **R console**. Submissions are checked one at a time, so a
large class may take some time. `timeout = 600` allows each submission up to ten
minutes to run. Increase this if an exercise needs longer downloads or package
installations. The remaining settings allow each submission 4 GB of memory and
two CPUs; these defaults should be a starting point for typical exercises.

The last two lines display the checking outcomes and open the overview in your
web browser. Review the reports, including any errors, before sending them to
students. Downloads and installed packages use disk space in `run_dir`, so make
sure your computer has enough free space.

If students need files supplied with the exercise, add
`workspace_template = "path/to/exercise-files"` to the call above, replacing the
example with the folder containing those files. The checker copies its contents
into each student's working folder.

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
directory. The checker cannot resume a partially completed run. Package only a completed,
reviewed batch using the checks below.


## 4. Put reports into the original Moodle folders and create the ZIP

Run the following after completing the review. It uses the original ZIP to
match reports to the correct students, including empty submission folders. It refuses to
package an incomplete batch or overwrite an earlier upload.

```r
zip_contents <- learncrimemapping:::package_moodle_feedback(
  submission_zip, run_dir, upload_dir, feedback_zip
)
zip_contents
```

Run this code in the **R console**. Inspect `zip_contents`: there should be exactly one nonempty report per Moodle
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

### Install the R package

Install the current `learncrimemapping` package from GitHub in the **R console**.
If you already use `devtools::install_github()`, that works too. You do not need
to download or keep a separate folder of package source files.

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("mpjashby/learncrimemapping", upgrade = "never")
```

This installs the package's usual dependencies in your computer's R library,
including `gifski`. The Docker image omits `gifski` because students rarely need
it. Packages installed by student code remain separate from this installation.

Restart R after installation. You can open whichever R project you use for
marking; all the following commands work from that project.

### Install Docker Desktop and prepare the checking environment

A Docker **image** is a prepared environment containing R, the checker and the
packages students normally need. Building it is a one-time preparation step;
it does not run or upload any student submissions.

1. Install [Docker Desktop](https://docs.docker.com/desktop/setup/install/mac-install/).
   On an Apple silicon Mac, choose the Apple silicon installer. If Docker Desktop
   is already installed, keep that installation.
2. Start Docker Desktop and wait until the engine is running.
3. Run this in the **R console**:

   ```r
   learncrimemapping:::build_checker_image()
   ```

The function uses files included in the installed package and automatically
reads its list of required R packages. It creates a temporary build folder and
removes that folder when finished. Your marking files are not included in the
build. Docker normally selects the correct platform for your computer, including
Apple silicon and Intel Macs.

Wait until R prints `Checker image ready: learncrimemapping-checker:local`
before marking. The first build can take considerable time because it downloads
and compiles packages. You need internet access and substantial free disk space.
Although the image uses a smaller R-only base, the full Docker build and its
final size have not yet been verified.

If the build fails, read the error shown in the R console and resolve it before
trying again. You can rerun the same command; Docker normally reuses completed
build steps. If Docker has been configured to use a different platform, specify
`platform = "linux/arm64"` for an Apple silicon Mac or
`platform = "linux/amd64"` for an Intel Mac.

Confirm that R can find the marking function:

```r
stopifnot(is.function(learncrimemapping:::check_submissions))
```

If this finishes without an error, you can follow steps 1–5. The Docker image is
available to all your R projects on this computer. You do not need to repeat the
build for each assignment.

### Packages and files available to student code

The build preinstalls the packages listed in `DESCRIPTION` Imports, except the
rarely needed `gifski`, plus their required R dependencies. The package names are
read automatically from `DESCRIPTION`; optional `Suggests` are not installed.
It installs the versions available on CRAN when you build the image. Later
marking runs reuse those versions until you rebuild. Each marking result records
which image was used.

The image also includes R, the checker and the supporting software needed for
spatial data, web downloads, fonts, images and package installation. It omits RStudio Server, TeX, Quarto,
Pandoc and Rust/Cargo. `rstudioapi` is an R interface package, not RStudio itself.
Documents have their enabled R chunks checked; they are not rendered.

Preinstalled libraries are read-only. Extra packages installed by student code,
including through `pacman::p_load()`, go into that student's
`/workspace/.sandbox/library`. The student code's home folder, temporary files and caches are also inside
its working folder. These files remain in the marking results for inspection and
are not shared with other students. Allow extra execution time for any package
compilation; missing system dependencies or network failures require assessor
review. For an occasional exercise requiring GIF encoding, prepare an image
with Rust/Cargo and `gifski` for that exercise.

Student code cannot access your computer's home folder, other submissions or
your computer's R package libraries through the checking environment. It can
still read the R installation, packages and supporting files inside the image.
Internet access remains enabled, including access to network services your
computer can reach. Provide only login details or access tokens needed for the
exercise. Keep the default Docker settings for student submissions;
`backend = "local"` runs code directly on your computer without this protection.

### When to rebuild

Reinstall the R package and rebuild the Docker image after changing the checker
code. Run `learncrimemapping:::build_checker_image()` again after changing the
preinstalled package list or choosing to update package versions. Repeat setup if Docker's images are
deleted or its stored data is reset. Each new computer needs its own image build
unless you transfer the existing image to it.

New student submissions, exercise resources, profile settings and output paths
do **not** require rebuilding the image.

### Optional settings for the build

Most instructors can use `build_checker_image()` without arguments. For a
custom exercise, `packages` can specify the R packages to preinstall and
`system_packages` can specify additional system software they require. The
checker always includes its core R packages. These options are intended for
instructors familiar with those dependencies.

If you change the image name using `image`, use that same name as
`container_image` when calling `check_submissions()`.

# Install project R packages onto Z: only (never the default C: user library).

r_lib <- "Z:/school study/pg/fyp/R/library"
dir.create(r_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(r_lib, .libPaths()))

options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  install.packages.compile.from.source = "never"
)

pkgs <- c(
  "remotes",
  "opera",
  "ggplot2",
  "dplyr",
  "readr",
  "scales",
  "tidyr",
  "zoo"
)

for (pkg in pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing ", pkg)
    install.packages(pkg, lib = r_lib, dependencies = TRUE)
  } else {
    message(pkg, " already available")
  }
}

if (!requireNamespace("AdaptiveConformal", quietly = TRUE)) {
  message("Installing AdaptiveConformal from GitHub")
  remotes::install_github(
    "herbps10/AdaptiveConformal",
    lib = r_lib,
    upgrade = "never",
    dependencies = TRUE
  )
} else {
  message("AdaptiveConformal already available")
}

message("Library paths:")
print(.libPaths())
message("AdaptiveConformal OK: ", requireNamespace("AdaptiveConformal", quietly = TRUE))

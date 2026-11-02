# Install packages for R 4.4.3 into the project library on Z:.

r_lib <- "Z:/school study/pg/fyp/R/library44"
dir.create(r_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(r_lib, .libPaths()))

options(
  repos = c(CRAN = "https://packagemanager.posit.co/cran/2025-01-15"),
  install.packages.compile.from.source = "never"
)

# Local opera Windows binary (removed from current CRAN).
opera_zip <- "Z:/R/downloads/opera_1.2.0.zip"
install.packages(opera_zip, repos = NULL, lib = r_lib)

pkgs <- c(
  "remotes",
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
}

message("Library paths:")
print(.libPaths())
message("opera: ", requireNamespace("opera", quietly = TRUE))
message("AdaptiveConformal: ", requireNamespace("AdaptiveConformal", quietly = TRUE))

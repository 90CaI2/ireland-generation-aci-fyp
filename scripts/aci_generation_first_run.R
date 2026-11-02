# First AdaptiveConformal run on Republic of Ireland total generation.
# Mirrors the flu case study in Susmann et al. (2024): black-box point
# forecasts wrapped by adaptive conformal methods.
#
# Locked design:
#   y_t  = IE Generation (MW)
#   step = 15 minutes
#   region = Republic of Ireland only
#   software = R + AdaptiveConformal

# Resolve project root from this script path (works under Rscript)
.args <- commandArgs(trailingOnly = FALSE)
.file <- sub("^--file=", "", .args[grep("^--file=", .args)])
root <- if (length(.file)) {
  normalizePath(file.path(dirname(.file), "../.."), winslash = "/", mustWork = TRUE)
} else {
  normalizePath("../..", winslash = "/", mustWork = TRUE)
}

r_lib <- file.path(root, "R/library44")
if (dir.exists(r_lib)) .libPaths(c(r_lib, .libPaths()))

suppressPackageStartupMessages({
  library(AdaptiveConformal)
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(scales)
})

processed <- file.path(root, "data/processed/ireland_qtr_hourly_2014_2025.csv")
fig_dir <- file.path(root, "figures")
note_dir <- file.path(root, "notes")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# ---- data ----
# First pass: all of 2024 at native 15-minute resolution.
# Full multi-year comparison can follow once the pipeline is stable.
raw <- read_csv(processed, show_col_types = FALSE)
dat <- raw %>%
  transmute(
    time = as.POSIXct(time, tz = "UTC"),
    y = generation
  ) %>%
  arrange(time) %>%
  filter(time >= as.POSIXct("2024-01-01", tz = "UTC"),
         time < as.POSIXct("2025-01-01", tz = "UTC")) %>%
  mutate(
    yhat = dplyr::lag(y, 96L), # seasonal naive: same quarter-hour yesterday
    residual = y - yhat
  ) %>%
  filter(!is.na(yhat))

message(sprintf("Analysis window: %s -> %s (%s rows)",
                min(dat$time), max(dat$time), comma(nrow(dat))))

# Warm-start / D calibration on first 14 days.
n_cal <- 96L * 14L
stopifnot(nrow(dat) > n_cal + 1000L)
D <- max(abs(dat$residual[seq_len(n_cal)]), na.rm = TRUE)
gamma <- D / sqrt(3)
message(sprintf("Calibration D = %.1f MW; SF-OGD/SAOCP gamma = %.2f", D, gamma))

y_fit <- dat$y
yhat_fit <- dat$yhat
alpha <- 0.9

gamma_grid_conf <- c(0.001, 0.002, 0.004, 0.008, 0.016, 0.032, 0.064, 0.128)

# Faster methods first. SAOCP is deferred: on ~35k fifteen-minute
# points it did not finish in a reasonable first-pass runtime.
methods <- list(
  `SF-OGD` = list(
    method = "SF-OGD",
    parameters = list(
      interval_constructor = "linear",
      D = D,
      gamma = gamma
    )
  ),
  DtACI = list(
    method = "DtACI",
    parameters = list(
      interval_constructor = "conformal",
      gamma_grid = gamma_grid_conf
    )
  ),
  AgACI = list(
    method = "AgACI",
    parameters = list(
      interval_constructor = "conformal",
      gamma_grid = gamma_grid_conf
    )
  )
)

extract_fit <- function(fit, method_name, times) {
  intervals <- as.data.frame(fit$intervals)
  colnames(intervals) <- c("lower", "upper")
  tibble(
    time = times,
    method = method_name,
    y = as.numeric(fit$Y),
    yhat = as.numeric(fit$predictions),
    lower = intervals$lower,
    upper = intervals$upper
  ) %>%
    mutate(
      covered = y >= lower & y <= upper,
      width = upper - lower,
      month = as.Date(format(time, "%Y-%m-01")),
      season = dplyr::case_when(
        as.integer(format(time, "%m")) %in% c(12, 1, 2) ~ "Winter",
        as.integer(format(time, "%m")) %in% c(3, 4, 5) ~ "Spring",
        as.integer(format(time, "%m")) %in% c(6, 7, 8) ~ "Summer",
        TRUE ~ "Autumn"
      )
    )
}

metrics_from <- function(df, burn_in = n_cal) {
  use <- df %>% filter(row_number() > burn_in)
  tibble(
    method = unique(df$method),
    n = nrow(use),
    coverage = mean(use$covered, na.rm = TRUE),
    mean_width = mean(use$width, na.rm = TRUE),
    path_length = sum(abs(diff(use$width)), na.rm = TRUE),
    winter_coverage = mean(use$covered[use$season == "Winter"], na.rm = TRUE),
    summer_coverage = mean(use$covered[use$season == "Summer"], na.rm = TRUE)
  )
}

fits_out <- list()
metric_rows <- list()

for (nm in names(methods)) {
  message("Fitting ", nm, " ...")
  t0 <- proc.time()[["elapsed"]]
  fit <- aci(
    Y = y_fit,
    predictions = yhat_fit,
    method = methods[[nm]]$method,
    alpha = alpha,
    parameters = methods[[nm]]$parameters
  )
  elapsed <- proc.time()[["elapsed"]] - t0
  message(sprintf("  done in %.1f s", elapsed))
  df <- extract_fit(fit, nm, dat$time)
  fits_out[[nm]] <- df
  m <- metrics_from(df)
  m$elapsed_sec <- elapsed
  metric_rows[[nm]] <- m
  print(summary(fit))
  # Checkpoint after each method so a later crash does not lose work.
  write_csv(bind_rows(metric_rows),
            file.path(root, "data/processed/aci_generation_metrics_2024.csv"))
}

metrics <- bind_rows(metric_rows)
print(metrics)

write_csv(metrics, file.path(root, "data/processed/aci_generation_metrics_2024.csv"))
all_intervals <- bind_rows(fits_out)
# Keep a lighter file for later plotting / inspection: daily means of width + coverage.
daily <- all_intervals %>%
  group_by(method, day = as.Date(time)) %>%
  summarise(
    coverage = mean(covered, na.rm = TRUE),
    mean_width = mean(width, na.rm = TRUE),
    y_mean = mean(y, na.rm = TRUE),
    .groups = "drop"
  )
write_csv(daily, file.path(root, "data/processed/aci_generation_daily_2024.csv"))

# ---- fixed-width baseline on the same forecast ----
half <- as.numeric(quantile(abs(dat$residual[seq_len(n_cal)]), probs = alpha, na.rm = TRUE))
fixed <- dat %>%
  mutate(
    method = "FixedWidth",
    lower = yhat - half,
    upper = yhat + half,
    covered = y >= lower & y <= upper,
    width = 2 * half,
    month = as.Date(format(time, "%Y-%m-01")),
    season = dplyr::case_when(
      as.integer(format(time, "%m")) %in% c(12, 1, 2) ~ "Winter",
      as.integer(format(time, "%m")) %in% c(3, 4, 5) ~ "Spring",
      as.integer(format(time, "%m")) %in% c(6, 7, 8) ~ "Summer",
      TRUE ~ "Autumn"
    )
  )
fixed_metrics <- metrics_from(fixed) %>% mutate(elapsed_sec = NA_real_)
metrics_all <- bind_rows(metrics, fixed_metrics)
write_csv(metrics_all, file.path(root, "data/processed/aci_generation_metrics_2024.csv"))

# ---- plots ----
theme_set(theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom"))

# Fortnight demo window (January 2024).
window <- as.POSIXct(c("2024-01-08", "2024-01-21 23:45:00"), tz = "UTC")
demo <- bind_rows(fits_out) %>%
  filter(time >= window[1], time <= window[2])

p1 <- ggplot(demo, aes(x = time)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#1B4F72", alpha = 0.2) +
  geom_line(aes(y = yhat), colour = "#5D6D7E", linewidth = 0.4) +
  geom_line(aes(y = y), colour = "#0E7C66", linewidth = 0.45) +
  facet_wrap(~method, ncol = 2) +
  labs(
    title = "Ireland total generation: 90% adaptive intervals",
    subtitle = "8–21 Jan 2024; black-box forecast = 1-day seasonal naive",
    x = NULL, y = "MW"
  )
ggsave(file.path(fig_dir, "11_aci_generation_fortnight.png"), p1,
       width = 11, height = 7, dpi = 150)

# Monthly coverage by method, including fixed width.
monthly <- bind_rows(
  lapply(fits_out, function(df) {
    df %>%
      filter(dplyr::row_number() > n_cal) %>%
      group_by(method, month) %>%
      summarise(coverage = mean(covered, na.rm = TRUE), .groups = "drop")
  }),
  fixed %>%
    filter(dplyr::row_number() > n_cal) %>%
    group_by(method, month) %>%
    summarise(coverage = mean(covered, na.rm = TRUE), .groups = "drop")
)

p2 <- ggplot(monthly, aes(x = month, y = coverage, colour = method)) +
  geom_hline(yintercept = alpha, linetype = "dashed", colour = "#7B241C") +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0.7, 1)) +
  labs(
    title = "Monthly empirical coverage of 90% intervals",
    subtitle = "Ireland IE Generation, 15-minute steps, 2024",
    x = NULL, y = "Coverage", colour = NULL
  )
ggsave(file.path(fig_dir, "12_aci_monthly_coverage.png"), p2,
       width = 11, height = 4.8, dpi = 150)

# Metric bar chart
metrics_plot <- metrics_all %>%
  mutate(method = factor(method, levels = c("FixedWidth", "AgACI", "DtACI", "SF-OGD"))) %>%
  pivot_longer(c(coverage, mean_width, winter_coverage, summer_coverage),
               names_to = "metric", values_to = "value")

p3 <- ggplot(metrics_plot %>% filter(metric %in% c("coverage", "winter_coverage", "summer_coverage")),
             aes(x = method, y = value, fill = metric)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = alpha, linetype = "dashed", colour = "#7B241C") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_fill_manual(
    values = c(coverage = "#1B4F72", winter_coverage = "#5D6D7E", summer_coverage = "#B9770E"),
    labels = c(coverage = "Overall", winter_coverage = "Winter", summer_coverage = "Summer")
  ) +
  labs(
    title = "Coverage by method (target 90%)",
    x = NULL, y = "Coverage", fill = NULL
  )
ggsave(file.path(fig_dir, "13_aci_coverage_by_method.png"), p3,
       width = 9.5, height = 4.6, dpi = 150)

p4 <- ggplot(metrics_all,
             aes(x = reorder(method, mean_width), y = mean_width)) +
  geom_col(fill = "#1B4F72", width = 0.7) +
  coord_flip() +
  labs(
    title = "Mean interval width",
    subtitle = "Smaller is better if coverage is near the target",
    x = NULL, y = "Mean width (MW)"
  )
ggsave(file.path(fig_dir, "14_aci_mean_width.png"), p4,
       width = 8, height = 4.2, dpi = 150)

# ---- notes ----
sink(file.path(note_dir, "aci_generation_first_run.txt"))
cat("AdaptiveConformal first run — Ireland total generation\n")
cat("Window: 2024-01-01 to 2024-12-31, 15-minute steps\n")
cat("Point forecast: seasonal naive lag 96 (same quarter-hour previous day)\n")
cat(sprintf("alpha = %.2f; calibration points = %d (%.1f days); D = %.1f MW; fixed half-width = %.1f MW\n\n",
            alpha, n_cal, n_cal / 96, D, half))
print(as.data.frame(metrics_all))
cat("\nWinter/summer gap (fixed width) shows why adaptive methods are needed.\n")
cat(sprintf("FixedWidth winter=%.1f%% summer=%.1f%%\n",
            100 * fixed_metrics$winter_coverage, 100 * fixed_metrics$summer_coverage))
cat("Figures: 11_aci_generation_fortnight.png, 12_aci_monthly_coverage.png,\n")
cat("         13_aci_coverage_by_method.png, 14_aci_mean_width.png\n")
sink()

# Also rebuild the multi-method monthly plot from daily checkpoints.
source(file.path(root, "R/scripts/rebuild_aci_summary_plots.R"), local = TRUE)

message("Done. Metrics written and figures 11-14 saved.")
print(metrics_all)

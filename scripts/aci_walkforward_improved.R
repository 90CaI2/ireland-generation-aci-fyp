# Walk-forward lagged+calendar predictor for Ireland total generation,
# then AdaptiveConformal comparison vs seasonal naive.
#
# Contamination rule: at forecast origin t, the point model is fit only on
# rows with time < t (rolling window of recent history). The model is
# retrained every `retrain_every` steps so the run is tractable.

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
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(scales)
})

fig_dir <- file.path(root, "figures")
proc_dir <- file.path(root, "data/processed")
note_dir <- file.path(root, "notes")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

alpha <- 0.9
retrain_every <- 96L * 7L   # weekly retrain
roll_days <- 365L           # use last year of history only
min_train <- 96L * 90L      # need at least 90 days before first forecast
eval_start <- as.POSIXct("2024-01-01", tz = "UTC")
eval_end <- as.POSIXct("2025-01-01", tz = "UTC")

# ---- load & feature engineering ----
raw <- read_csv(file.path(proc_dir, "ireland_qtr_hourly_2014_2025.csv"),
                show_col_types = FALSE)

dat <- raw %>%
  transmute(
    time = as.POSIXct(time, tz = "UTC"),
    y = as.numeric(generation)
  ) %>%
  arrange(time) %>%
  filter(!is.na(y)) %>%
  mutate(
    lag1 = dplyr::lag(y, 1L),
    lag4 = dplyr::lag(y, 4L),
    lag96 = dplyr::lag(y, 96L),
    lag672 = dplyr::lag(y, 672L),
    hour = as.integer(format(time, "%H")),
    minute = as.integer(format(time, "%M")),
    hod = hour + minute / 60,
    dow = as.integer(format(time, "%u")), # 1=Mon .. 7=Sun
    month = as.integer(format(time, "%m")),
    # Fourier terms for daily cycle (known at forecast time)
    sin_day = sin(2 * pi * hod / 24),
    cos_day = cos(2 * pi * hod / 24),
    sin_week = sin(2 * pi * (dow - 1) / 7),
    cos_week = cos(2 * pi * (dow - 1) / 7),
    yhat_naive = lag96
  ) %>%
  filter(!is.na(lag672))

feature_cols <- c(
  "lag1", "lag4", "lag96", "lag672",
  "hod", "dow", "month",
  "sin_day", "cos_day", "sin_week", "cos_week"
)

form <- as.formula(paste("y ~", paste(feature_cols, collapse = " + ")))

# ---- walk-forward linear predictor (causal) ----
message("Building walk-forward linear forecasts...")
n <- nrow(dat)
yhat_wf <- rep(NA_real_, n)
eval_idx <- which(dat$time >= eval_start & dat$time < eval_end)
stopifnot(length(eval_idx) > 1000L)

first_eval <- min(eval_idx)
# Ensure enough history before first evaluation point
stopifnot(first_eval > min_train)

model <- NULL
last_train_end <- 0L
t0 <- proc.time()[["elapsed"]]

for (i in eval_idx) {
  # Retrain if needed: only using rows strictly before i
  if (is.null(model) || (i - last_train_end) >= retrain_every || i == first_eval) {
    train_end <- i - 1L
    train_start <- max(1L, train_end - roll_days * 96L + 1L)
    train <- dat[train_start:train_end, , drop = FALSE]
    model <- lm(form, data = train)
    last_train_end <- i
  }
  yhat_wf[i] <- as.numeric(predict(model, newdata = dat[i, , drop = FALSE]))
}
message(sprintf("Walk-forward done in %.1f s", proc.time()[["elapsed"]] - t0))

dat$yhat_wf <- yhat_wf

# Evaluation slice for 2024 with both predictors available
eval_df <- dat %>%
  filter(time >= eval_start, time < eval_end, !is.na(yhat_wf), !is.na(yhat_naive)) %>%
  mutate(
    resid_naive = y - yhat_naive,
    resid_wf = y - yhat_wf,
    season = dplyr::case_when(
      as.integer(format(time, "%m")) %in% c(12, 1, 2) ~ "Winter",
      as.integer(format(time, "%m")) %in% c(3, 4, 5) ~ "Spring",
      as.integer(format(time, "%m")) %in% c(6, 7, 8) ~ "Summer",
      TRUE ~ "Autumn"
    )
  )

rmse <- function(e) sqrt(mean(e^2, na.rm = TRUE))
mae <- function(e) mean(abs(e), na.rm = TRUE)

pred_metrics <- eval_df %>%
  summarise(
    n = n(),
    naive_rmse = rmse(resid_naive),
    naive_mae = mae(resid_naive),
    wf_rmse = rmse(resid_wf),
    wf_mae = mae(resid_wf),
    naive_summer_mae = mae(resid_naive[season == "Summer"]),
    wf_summer_mae = mae(resid_wf[season == "Summer"]),
    naive_winter_mae = mae(resid_naive[season == "Winter"]),
    wf_winter_mae = mae(resid_wf[season == "Winter"])
  )
print(pred_metrics)
write_csv(pred_metrics, file.path(proc_dir, "point_forecast_compare_2024.csv"))
write_csv(
  eval_df %>% select(time, y, yhat_naive, yhat_wf, season),
  file.path(proc_dir, "point_forecasts_2024.csv")
)

# ---- ACI on both predictors ----
run_aci_suite <- function(y, yhat, times, label) {
  n_cal <- 96L * 14L
  resid <- y - yhat
  D <- max(abs(resid[seq_len(n_cal)]), na.rm = TRUE)
  gamma <- D / sqrt(3)
  gamma_grid <- c(0.001, 0.002, 0.004, 0.008, 0.016, 0.032, 0.064, 0.128)

  methods <- list(
    `SF-OGD` = list(method = "SF-OGD",
                    parameters = list(interval_constructor = "linear", D = D, gamma = gamma)),
    DtACI = list(method = "DtACI",
                 parameters = list(interval_constructor = "conformal", gamma_grid = gamma_grid)),
    AgACI = list(method = "AgACI",
                 parameters = list(interval_constructor = "conformal", gamma_grid = gamma_grid))
  )

  rows <- list()
  fits <- list()

  # Fixed-width baseline for this predictor (calibrated on warm-up only)
  half <- as.numeric(quantile(abs(resid[seq_len(n_cal)]), probs = alpha, na.rm = TRUE))

  for (nm in names(methods)) {
    message(sprintf("[%s] Fitting %s ...", label, nm))
    t1 <- proc.time()[["elapsed"]]
    fit <- aci(
      Y = y,
      predictions = yhat,
      method = methods[[nm]]$method,
      alpha = alpha,
      parameters = methods[[nm]]$parameters
    )
    elapsed <- proc.time()[["elapsed"]] - t1
    message(sprintf("  done in %.1f s", elapsed))
    iv <- as.data.frame(fit$intervals)
    colnames(iv) <- c("lower", "upper")
    df <- tibble(
      time = times,
      predictor = label,
      method = nm,
      y = as.numeric(fit$Y),
      yhat = as.numeric(fit$predictions),
      lower = iv$lower,
      upper = iv$upper
    ) %>%
      mutate(
        covered = y >= lower & y <= upper,
        width = upper - lower,
        season = dplyr::case_when(
          as.integer(format(time, "%m")) %in% c(12, 1, 2) ~ "Winter",
          as.integer(format(time, "%m")) %in% c(3, 4, 5) ~ "Spring",
          as.integer(format(time, "%m")) %in% c(6, 7, 8) ~ "Summer",
          TRUE ~ "Autumn"
        )
      )
    use <- df %>% filter(dplyr::row_number() > n_cal)
    metrics <- tibble(
      predictor = label,
      method = nm,
      n = nrow(use),
      coverage = mean(use$covered),
      mean_width = mean(use$width),
      median_width = median(use$width),
      winter_coverage = mean(use$covered[use$season == "Winter"]),
      summer_coverage = mean(use$covered[use$season == "Summer"]),
      summer_mean_width = mean(use$width[use$season == "Summer"]),
      winter_mean_width = mean(use$width[use$season == "Winter"]),
      elapsed_sec = elapsed,
      D = D
    )
    rows[[nm]] <- metrics
    fits[[nm]] <- df
    write_csv(bind_rows(rows), file.path(proc_dir, paste0("aci_metrics_tmp_", label, ".csv")))
  }

  fixed_df <- tibble(
    time = times,
    predictor = label,
    method = "FixedWidth",
    y = y,
    yhat = yhat,
    lower = yhat - half,
    upper = yhat + half
  ) %>%
    mutate(
      covered = y >= lower & y <= upper,
      width = 2 * half,
      season = dplyr::case_when(
        as.integer(format(time, "%m")) %in% c(12, 1, 2) ~ "Winter",
        as.integer(format(time, "%m")) %in% c(3, 4, 5) ~ "Spring",
        as.integer(format(time, "%m")) %in% c(6, 7, 8) ~ "Summer",
        TRUE ~ "Autumn"
      )
    )
  use_f <- fixed_df %>% filter(dplyr::row_number() > n_cal)
  metrics_f <- tibble(
    predictor = label,
    method = "FixedWidth",
    n = nrow(use_f),
    coverage = mean(use_f$covered),
    mean_width = mean(use_f$width),
    median_width = median(use_f$width),
    winter_coverage = mean(use_f$covered[use_f$season == "Winter"]),
    summer_coverage = mean(use_f$covered[use_f$season == "Summer"]),
    summer_mean_width = mean(use_f$width[use_f$season == "Summer"]),
    winter_mean_width = mean(use_f$width[use_f$season == "Winter"]),
    elapsed_sec = NA_real_,
    D = D
  )

  list(
    metrics = bind_rows(bind_rows(rows), metrics_f),
    fits = fits,
    fixed_half = half
  )
}

naive_run <- run_aci_suite(eval_df$y, eval_df$yhat_naive, eval_df$time, "seasonal_naive")
wf_run <- run_aci_suite(eval_df$y, eval_df$yhat_wf, eval_df$time, "walkforward_lm")

metrics_all <- bind_rows(naive_run$metrics, wf_run$metrics)
write_csv(metrics_all, file.path(proc_dir, "aci_generation_metrics_2024_improved.csv"))
print(metrics_all)

# ---- plots ----
invisible(try(Sys.setlocale("LC_TIME", "English_United States.1252"), silent = TRUE))
invisible(try(Sys.setlocale("LC_TIME", "English"), silent = TRUE))

english_time_scale <- function() {
  scale_x_datetime(
    date_breaks = "3 days",
    date_labels = "%d %b %Y",
    expand = expansion(mult = c(0.01, 0.01))
  )
}

theme_set(theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom"))

# 15: point forecast comparison fortnight (summer stress)
summer_window <- as.POSIXct(c("2024-07-08", "2024-07-21 23:45:00"), tz = "UTC")
p15 <- eval_df %>%
  filter(time >= summer_window[1], time <= summer_window[2]) %>%
  select(time, y, yhat_naive, yhat_wf) %>%
  pivot_longer(-time, names_to = "series", values_to = "mw") %>%
  mutate(series = recode(series,
                         y = "Observed",
                         yhat_naive = "Seasonal naive",
                         yhat_wf = "Walk-forward LM")) %>%
  ggplot(aes(time, mw, colour = series)) +
  geom_line(linewidth = 0.45) +
  english_time_scale() +
  scale_colour_manual(values = c(
    Observed = "#0E7C66",
    `Seasonal naive` = "#5D6D7E",
    `Walk-forward LM` = "#1B4F72"
  )) +
  labs(
    title = "Point forecasts vs observed generation",
    subtitle = "Exact window: 8 July 2024 00:00 – 21 July 2024 23:45 UTC",
    x = NULL, y = "MW", colour = NULL
  )
ggsave(file.path(fig_dir, "15_point_forecast_summer_fortnight.png"), p15,
       width = 11, height = 4.6, dpi = 150)

# 16: ACI with improved predictor (mentor-requested three layers)
winter_window <- as.POSIXct(c("2024-01-08", "2024-01-21 23:45:00"), tz = "UTC")
demo <- bind_rows(wf_run$fits) %>%
  filter(time >= winter_window[1], time <= winter_window[2])

p16 <- ggplot(demo, aes(x = time)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#1B4F72", alpha = 0.2) +
  geom_line(aes(y = yhat), colour = "#5D6D7E", linewidth = 0.4) +
  geom_line(aes(y = y), colour = "#0E7C66", linewidth = 0.45) +
  facet_wrap(~method, ncol = 2) +
  english_time_scale() +
  labs(
    title = "Observed, walk-forward prediction, and 90% ACI intervals",
    subtitle = "Exact window: 8 Jan 2024 00:00 – 21 Jan 2024 23:45 UTC; calendar+lag LM",
    x = NULL, y = "MW"
  )
ggsave(file.path(fig_dir, "16_aci_walkforward_winter_fortnight.png"), p16,
       width = 11, height = 7, dpi = 150)

# Summer ACI with improved predictor (addresses "large summer intervals")
demo_s <- bind_rows(wf_run$fits) %>%
  filter(time >= summer_window[1], time <= summer_window[2])
p16b <- ggplot(demo_s, aes(x = time)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#1B4F72", alpha = 0.2) +
  geom_line(aes(y = yhat), colour = "#5D6D7E", linewidth = 0.4) +
  geom_line(aes(y = y), colour = "#0E7C66", linewidth = 0.45) +
  facet_wrap(~method, ncol = 2) +
  english_time_scale() +
  labs(
    title = "Observed, walk-forward prediction, and 90% ACI intervals",
    subtitle = "Exact window: 8 July 2024 00:00 – 21 July 2024 23:45 UTC; calendar+lag LM",
    x = NULL, y = "MW"
  )
ggsave(file.path(fig_dir, "17_aci_walkforward_summer_fortnight.png"), p16b,
       width = 11, height = 7, dpi = 150)

# 18: Zaffran-style coverage vs mean width
p18 <- metrics_all %>%
  filter(method != "FixedWidth") %>%
  ggplot(aes(coverage, mean_width, colour = predictor, shape = method)) +
  geom_vline(xintercept = alpha, linetype = "dashed", colour = "#7B241C") +
  geom_point(size = 3) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0.85, 0.98)) +
  labs(
    title = "Validity–efficiency trade-off (Zaffran-style)",
    subtitle = "2024 Ireland total generation, 15-minute steps, target coverage 90%",
    x = "Empirical coverage", y = "Mean interval width (MW)",
    colour = "Base predictor", shape = "ACI method"
  )
ggsave(file.path(fig_dir, "18_coverage_vs_width_by_predictor.png"), p18,
       width = 9.5, height = 5.2, dpi = 150)

# 19: mean width comparison bars
p19 <- metrics_all %>%
  mutate(label = paste(predictor, method, sep = " / ")) %>%
  ggplot(aes(x = reorder(label, mean_width), y = mean_width, fill = predictor)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c(seasonal_naive = "#5D6D7E", walkforward_lm = "#1B4F72")) +
  labs(
    title = "Mean ACI interval width by base predictor",
    subtitle = "Smaller is better if coverage stays near 90%",
    x = NULL, y = "Mean width (MW)", fill = NULL
  )
ggsave(file.path(fig_dir, "19_mean_width_by_predictor.png"), p19,
       width = 10, height = 5.2, dpi = 150)

# ---- notes ----
sink(file.path(note_dir, "improved_predictor_aci_summary.txt"))
cat("Improved walk-forward predictor + ACI comparison\n")
cat("Eval window: 2024; step: 15-min; target: IE Generation\n")
cat("Walk-forward LM: weekly retrain, rolling 365-day history, lags 1/4/96/672 + calendar\n")
cat("No future contamination: each model fit only on time < forecast origin\n\n")
cat("Point forecast metrics:\n")
print(as.data.frame(pred_metrics))
cat("\nACI metrics:\n")
print(as.data.frame(metrics_all))
cat("\nFigures: 15-19 in figures/\n")
sink()

message("Done. Wrote metrics and figures 15-19.")

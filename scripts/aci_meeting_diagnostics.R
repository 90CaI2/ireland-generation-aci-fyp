# Meeting 19 Oct 2026 push-forward:
# 1) Re-fit ACI on saved 2024 point forecasts (naive + walk-forward LM)
# 2) Add wind-lag walk-forward LM (causal) and ACI
# 3) Path length, weekday/weekend, hour-of-day coverage (Zaffran-style)
# 4) Regenerate figures with English date axes and exact calendar dates

.args <- commandArgs(trailingOnly = FALSE)
.file <- sub("^--file=", "", .args[grep("^--file=", .args)])
root <- if (length(.file)) {
  normalizePath(file.path(dirname(.file), "../.."), winslash = "/", mustWork = TRUE)
} else {
  normalizePath("../..", winslash = "/", mustWork = TRUE)
}

r_lib <- file.path(root, "R/library44")
if (dir.exists(r_lib)) .libPaths(c(r_lib, .libPaths()))

# Force English month names on Windows (avoids non-English locale on axes)
invisible(try(Sys.setlocale("LC_TIME", "English_United States.1252"), silent = TRUE))
invisible(try(Sys.setlocale("LC_TIME", "English"), silent = TRUE))

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
n_cal <- 96L * 14L
retrain_every <- 96L * 7L
roll_days <- 365L
min_train <- 96L * 90L
eval_start <- as.POSIXct("2024-01-01", tz = "UTC")
eval_end <- as.POSIXct("2025-01-01", tz = "UTC")

english_time_scale <- function() {
  scale_x_datetime(
    date_breaks = "3 days",
    date_labels = "%d %b %Y",
    expand = expansion(mult = c(0.01, 0.01))
  )
}

season_of <- function(time) {
  dplyr::case_when(
    as.integer(format(time, "%m")) %in% c(12, 1, 2) ~ "Winter",
    as.integer(format(time, "%m")) %in% c(3, 4, 5) ~ "Spring",
    as.integer(format(time, "%m")) %in% c(6, 7, 8) ~ "Summer",
    TRUE ~ "Autumn"
  )
}

# ---- load series + build wind-aware walk-forward ----
raw <- read_csv(file.path(proc_dir, "ireland_qtr_hourly_2014_2025.csv"),
                show_col_types = FALSE)

dat <- raw %>%
  transmute(
    time = as.POSIXct(time, tz = "UTC"),
    y = as.numeric(generation),
    wind = as.numeric(wind)
  ) %>%
  arrange(time) %>%
  filter(!is.na(y)) %>%
  mutate(
    lag1 = dplyr::lag(y, 1L),
    lag4 = dplyr::lag(y, 4L),
    lag96 = dplyr::lag(y, 96L),
    lag672 = dplyr::lag(y, 672L),
    wind_lag1 = dplyr::lag(wind, 1L),
    wind_lag4 = dplyr::lag(wind, 4L),
    wind_lag96 = dplyr::lag(wind, 96L),
    hour = as.integer(format(time, "%H")),
    minute = as.integer(format(time, "%M")),
    hod = hour + minute / 60,
    dow = as.integer(format(time, "%u")),
    month = as.integer(format(time, "%m")),
    sin_day = sin(2 * pi * hod / 24),
    cos_day = cos(2 * pi * hod / 24),
    sin_week = sin(2 * pi * (dow - 1) / 7),
    cos_week = cos(2 * pi * (dow - 1) / 7),
    yhat_naive = lag96
  ) %>%
  filter(!is.na(lag672), !is.na(wind_lag96))

base_cols <- c(
  "lag1", "lag4", "lag96", "lag672",
  "hod", "dow", "month",
  "sin_day", "cos_day", "sin_week", "cos_week"
)
wind_cols <- c(base_cols, "wind_lag1", "wind_lag4", "wind_lag96")

walk_forward_lm <- function(dat, feature_cols, label) {
  form <- as.formula(paste("y ~", paste(feature_cols, collapse = " + ")))
  n <- nrow(dat)
  yhat <- rep(NA_real_, n)
  eval_idx <- which(dat$time >= eval_start & dat$time < eval_end)
  first_eval <- min(eval_idx)
  stopifnot(first_eval > min_train)
  model <- NULL
  last_train_end <- 0L
  t0 <- proc.time()[["elapsed"]]
  for (i in eval_idx) {
    if (is.null(model) || (i - last_train_end) >= retrain_every || i == first_eval) {
      train_end <- i - 1L
      train_start <- max(1L, train_end - roll_days * 96L + 1L)
      train <- dat[train_start:train_end, , drop = FALSE]
      model <- lm(form, data = train)
      last_train_end <- i
    }
    yhat[i] <- as.numeric(predict(model, newdata = dat[i, , drop = FALSE]))
  }
  message(sprintf("[%s] walk-forward done in %.1f s", label, proc.time()[["elapsed"]] - t0))
  yhat
}

# Prefer cached calendar LM forecasts if present; rebuild wind LM always
pf <- read_csv(file.path(proc_dir, "point_forecasts_2024.csv"), show_col_types = FALSE) %>%
  mutate(time = as.POSIXct(time, tz = "UTC"))

message("Building walk-forward LM with wind lags...")
dat$yhat_wf_wind <- walk_forward_lm(dat, wind_cols, "walkforward_lm_wind")

# Align calendar LM from cache (avoids another 50s loop if lengths match)
eval_df <- dat %>%
  filter(time >= eval_start, time < eval_end) %>%
  left_join(pf %>% select(time, yhat_wf), by = "time") %>%
  mutate(
    yhat_wf = ifelse(is.na(yhat_wf), NA_real_, yhat_wf),
    resid_naive = y - yhat_naive,
    resid_wf = y - yhat_wf,
    resid_wind = y - yhat_wf_wind,
    season = season_of(time),
    weekday = format(time, "%A", tz = "UTC"),
    is_weekend = dow >= 6L,
    hour_bin = dplyr::case_when(
      hod >= 0 & hod < 6 ~ "00-06 night",
      hod >= 6 & hod < 12 ~ "06-12 morning",
      hod >= 12 & hod < 18 ~ "12-18 afternoon",
      TRUE ~ "18-24 evening"
    )
  ) %>%
  filter(!is.na(yhat_wf), !is.na(yhat_wf_wind), !is.na(yhat_naive))

rmse <- function(e) sqrt(mean(e^2, na.rm = TRUE))
mae <- function(e) mean(abs(e), na.rm = TRUE)

pred_metrics <- tibble(
  predictor = c("seasonal_naive", "walkforward_lm", "walkforward_lm_wind"),
  rmse = c(rmse(eval_df$resid_naive), rmse(eval_df$resid_wf), rmse(eval_df$resid_wind)),
  mae = c(mae(eval_df$resid_naive), mae(eval_df$resid_wf), mae(eval_df$resid_wind)),
  summer_mae = c(
    mae(eval_df$resid_naive[eval_df$season == "Summer"]),
    mae(eval_df$resid_wf[eval_df$season == "Summer"]),
    mae(eval_df$resid_wind[eval_df$season == "Summer"])
  ),
  winter_mae = c(
    mae(eval_df$resid_naive[eval_df$season == "Winter"]),
    mae(eval_df$resid_wf[eval_df$season == "Winter"]),
    mae(eval_df$resid_wind[eval_df$season == "Winter"])
  )
)
print(pred_metrics)
write_csv(pred_metrics, file.path(proc_dir, "point_forecast_compare_2024_with_wind.csv"))

# ---- ACI suite ----
run_aci_suite <- function(y, yhat, times, meta, label) {
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
  half <- as.numeric(quantile(abs(resid[seq_len(n_cal)]), probs = alpha, na.rm = TRUE))

  for (nm in names(methods)) {
    message(sprintf("[%s] Fitting %s ...", label, nm))
    t1 <- proc.time()[["elapsed"]]
    fit <- aci(
      Y = y, predictions = yhat,
      method = methods[[nm]]$method, alpha = alpha,
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
        covered = is.finite(lower) & is.finite(upper) & y >= lower & y <= upper,
        width = upper - lower,
        infinite = !is.finite(lower) | !is.finite(upper),
        season = season_of(time)
      ) %>%
      bind_cols(meta)
    use <- df %>% filter(dplyr::row_number() > n_cal)
    path_len <- sum(abs(diff(use$width[is.finite(use$width)])), na.rm = TRUE)
    metrics <- tibble(
      predictor = label,
      method = nm,
      n = nrow(use),
      coverage = mean(use$covered, na.rm = TRUE),
      mean_width = mean(use$width[is.finite(use$width)], na.rm = TRUE),
      median_width = median(use$width[is.finite(use$width)], na.rm = TRUE),
      path_length = path_len,
      mean_abs_width_change = path_len / max(1, sum(is.finite(use$width)) - 1),
      pct_infinite = mean(use$infinite),
      winter_coverage = mean(use$covered[use$season == "Winter"], na.rm = TRUE),
      summer_coverage = mean(use$covered[use$season == "Summer"], na.rm = TRUE),
      summer_mean_width = mean(use$width[use$season == "Summer" & is.finite(use$width)], na.rm = TRUE),
      winter_mean_width = mean(use$width[use$season == "Winter" & is.finite(use$width)], na.rm = TRUE),
      weekend_coverage = mean(use$covered[use$is_weekend], na.rm = TRUE),
      weekday_coverage = mean(use$covered[!use$is_weekend], na.rm = TRUE),
      elapsed_sec = elapsed,
      D = D
    )
    rows[[nm]] <- metrics
    fits[[nm]] <- df
    write_csv(bind_rows(rows), file.path(proc_dir, paste0("aci_metrics_tmp_", label, ".csv")))
  }

  fixed_df <- tibble(
    time = times, predictor = label, method = "FixedWidth",
    y = y, yhat = yhat, lower = yhat - half, upper = yhat + half
  ) %>%
    mutate(
      covered = y >= lower & y <= upper,
      width = 2 * half,
      infinite = FALSE,
      season = season_of(time)
    ) %>%
    bind_cols(meta)
  use_f <- fixed_df %>% filter(dplyr::row_number() > n_cal)
  metrics_f <- tibble(
    predictor = label, method = "FixedWidth", n = nrow(use_f),
    coverage = mean(use_f$covered),
    mean_width = mean(use_f$width),
    median_width = median(use_f$width),
    path_length = 0,
    mean_abs_width_change = 0,
    pct_infinite = 0,
    winter_coverage = mean(use_f$covered[use_f$season == "Winter"]),
    summer_coverage = mean(use_f$covered[use_f$season == "Summer"]),
    summer_mean_width = mean(use_f$width[use_f$season == "Summer"]),
    winter_mean_width = mean(use_f$width[use_f$season == "Winter"]),
    weekend_coverage = mean(use_f$covered[use_f$is_weekend]),
    weekday_coverage = mean(use_f$covered[!use_f$is_weekend]),
    elapsed_sec = NA_real_, D = D
  )

  list(metrics = bind_rows(bind_rows(rows), metrics_f), fits = fits, fixed = fixed_df)
}

meta <- eval_df %>% select(dow, is_weekend, hour_bin, weekday)

naive_run <- run_aci_suite(eval_df$y, eval_df$yhat_naive, eval_df$time, meta, "seasonal_naive")
wf_run <- run_aci_suite(eval_df$y, eval_df$yhat_wf, eval_df$time, meta, "walkforward_lm")
wind_run <- run_aci_suite(eval_df$y, eval_df$yhat_wf_wind, eval_df$time, meta, "walkforward_lm_wind")

metrics_all <- bind_rows(naive_run$metrics, wf_run$metrics, wind_run$metrics)
write_csv(metrics_all, file.path(proc_dir, "aci_generation_metrics_2024_meeting.csv"))
print(metrics_all)

intervals_all <- bind_rows(
  bind_rows(naive_run$fits),
  bind_rows(wf_run$fits),
  bind_rows(wind_run$fits)
)
write_csv(intervals_all, file.path(proc_dir, "aci_intervals_2024_meeting.csv"))

# Conditional coverage tables (post warm-up)
cond_hour <- intervals_all %>%
  arrange(predictor, method, time) %>%
  group_by(predictor, method) %>%
  mutate(idx = dplyr::row_number()) %>%
  ungroup() %>%
  filter(idx > n_cal) %>%
  group_by(predictor, method, hour_bin) %>%
  summarise(
    n = n(),
    coverage = mean(covered, na.rm = TRUE),
    mean_width = mean(width[is.finite(width)], na.rm = TRUE),
    .groups = "drop"
  )
write_csv(cond_hour, file.path(proc_dir, "aci_coverage_by_hourbin_2024.csv"))

cond_dow <- intervals_all %>%
  arrange(predictor, method, time) %>%
  group_by(predictor, method) %>%
  mutate(idx = dplyr::row_number()) %>%
  ungroup() %>%
  filter(idx > n_cal) %>%
  group_by(predictor, method, weekday) %>%
  summarise(
    n = n(),
    coverage = mean(covered, na.rm = TRUE),
    mean_width = mean(width[is.finite(width)], na.rm = TRUE),
    .groups = "drop"
  )
write_csv(cond_dow, file.path(proc_dir, "aci_coverage_by_weekday_2024.csv"))

# ---- plots (English dates) ----
theme_set(theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom"))

summer_window <- as.POSIXct(c("2024-07-08 00:00:00", "2024-07-21 23:45:00"), tz = "UTC")
winter_window <- as.POSIXct(c("2024-01-08 00:00:00", "2024-01-21 23:45:00"), tz = "UTC")

# 15: point forecasts summer
p15 <- eval_df %>%
  filter(time >= summer_window[1], time <= summer_window[2]) %>%
  select(time, y, yhat_naive, yhat_wf, yhat_wf_wind) %>%
  pivot_longer(-time, names_to = "series", values_to = "mw") %>%
  mutate(series = recode(series,
                         y = "Observed",
                         yhat_naive = "Seasonal naive",
                         yhat_wf = "Walk-forward LM",
                         yhat_wf_wind = "Walk-forward LM + wind lags")) %>%
  ggplot(aes(time, mw, colour = series)) +
  geom_line(linewidth = 0.4) +
  english_time_scale() +
  scale_colour_manual(values = c(
    Observed = "#0E7C66",
    `Seasonal naive` = "#5D6D7E",
    `Walk-forward LM` = "#1B4F72",
    `Walk-forward LM + wind lags` = "#B9770E"
  )) +
  labs(
    title = "Point forecasts vs observed generation",
    subtitle = "Exact window: 8 July 2024 00:00 – 21 July 2024 23:45 UTC",
    x = NULL, y = "MW", colour = NULL
  )
ggsave(file.path(fig_dir, "15_point_forecast_summer_fortnight.png"), p15,
       width = 11, height = 4.8, dpi = 150)

# 16 / 17: ACI ribbons for walkforward_lm
plot_aci_fortnight <- function(fits, window, file, title_sub) {
  demo <- bind_rows(fits) %>%
    filter(time >= window[1], time <= window[2])
  p <- ggplot(demo, aes(x = time)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#1B4F72", alpha = 0.2) +
    geom_line(aes(y = yhat), colour = "#5D6D7E", linewidth = 0.35) +
    geom_line(aes(y = y), colour = "#0E7C66", linewidth = 0.4) +
    facet_wrap(~method, ncol = 2) +
    english_time_scale() +
    labs(
      title = "Observed, walk-forward prediction, and 90% ACI intervals",
      subtitle = title_sub,
      x = NULL, y = "MW"
    )
  ggsave(file.path(fig_dir, file), p, width = 11, height = 7, dpi = 150)
}

plot_aci_fortnight(
  wf_run$fits, winter_window,
  "16_aci_walkforward_winter_fortnight.png",
  "Exact window: 8 Jan 2024 00:00 – 21 Jan 2024 23:45 UTC; base = calendar+lag LM"
)
plot_aci_fortnight(
  wf_run$fits, summer_window,
  "17_aci_walkforward_summer_fortnight.png",
  "Exact window: 8 July 2024 00:00 – 21 July 2024 23:45 UTC; base = calendar+lag LM"
)

# 18: coverage vs width
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
       width = 9.8, height = 5.2, dpi = 150)

# 19: mean width bars
p19 <- metrics_all %>%
  mutate(label = paste(predictor, method, sep = " / ")) %>%
  ggplot(aes(x = reorder(label, mean_width), y = mean_width, fill = predictor)) +
  geom_col(width = 0.7) +
  coord_flip() +
  labs(
    title = "Mean ACI interval width by base predictor",
    subtitle = "Smaller is better if coverage stays near 90%",
    x = NULL, y = "Mean width (MW)", fill = NULL
  )
ggsave(file.path(fig_dir, "19_mean_width_by_predictor.png"), p19,
       width = 10.5, height = 5.8, dpi = 150)

# 20: path length (Susmann metric)
p20 <- metrics_all %>%
  filter(method != "FixedWidth") %>%
  ggplot(aes(x = reorder(paste(predictor, method), path_length), y = path_length, fill = predictor)) +
  geom_col(width = 0.7) +
  coord_flip() +
  labs(
    title = "Interval path length (stability)",
    subtitle = "Sum of absolute width changes over 2024 after warm-up; lower = less oscillation",
    x = NULL, y = "Path length (MW)", fill = NULL
  )
ggsave(file.path(fig_dir, "20_path_length_by_method.png"), p20,
       width = 10.5, height = 5.2, dpi = 150)

# 21: weekday coverage heatmap for best predictor family
p21 <- cond_dow %>%
  filter(predictor == "walkforward_lm", method != "FixedWidth") %>%
  mutate(
    weekday = factor(weekday, levels = c(
      "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"
    ))
  ) %>%
  ggplot(aes(weekday, method, fill = coverage)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.1f%%", 100 * coverage)), size = 3.2) +
  scale_fill_gradient2(midpoint = alpha, low = "#B03A2E", mid = "#F7F9F9", high = "#1A5276",
                       labels = percent_format(accuracy = 1)) +
  labs(
    title = "Weekday coverage — walk-forward LM + ACI",
    subtitle = "Target 90%; Zaffran-style conditional check (analogous to their weekday bias on prices)",
    x = NULL, y = NULL, fill = "Coverage"
  )
ggsave(file.path(fig_dir, "21_weekday_coverage_walkforward.png"), p21,
       width = 10, height = 4.2, dpi = 150)

# 22: hour-bin coverage
p22 <- cond_hour %>%
  filter(predictor %in% c("walkforward_lm", "walkforward_lm_wind"), method == "DtACI") %>%
  ggplot(aes(hour_bin, coverage, fill = predictor)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = alpha, linetype = "dashed", colour = "#7B241C") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0.85, 0.98)) +
  labs(
    title = "Coverage by time of day (DtACI)",
    subtitle = "2024 after 14-day warm-up; dashed line = 90% target",
    x = NULL, y = "Empirical coverage", fill = "Predictor"
  )
ggsave(file.path(fig_dir, "22_hourbin_coverage_dtaci.png"), p22,
       width = 9.5, height = 4.8, dpi = 150)

# 23: summer ACI with wind-aware predictor
plot_aci_fortnight(
  wind_run$fits, summer_window,
  "23_aci_wind_summer_fortnight.png",
  "Exact window: 8 July 2024 00:00 – 21 July 2024 23:45 UTC; base = LM + lagged wind"
)

# notes dump
sink(file.path(note_dir, "meeting_diagnostics_summary.txt"))
cat("Meeting diagnostics 19 Oct 2026\n\n")
cat("Point forecast metrics:\n")
print(as.data.frame(pred_metrics))
cat("\nACI metrics (incl. path length, weekday):\n")
print(as.data.frame(metrics_all))
cat("\nFigures: 15-23 regenerated with English date labels.\n")
sink()

message("Done. Wrote metrics, intervals, and figures 15-23.")

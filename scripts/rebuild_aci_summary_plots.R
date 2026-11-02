# Rebuild ACI summary figures from saved daily metrics + a fresh fixed-width series.

.libPaths(c("Z:/school study/pg/fyp/R/library44", .libPaths()))
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(scales)
})

root <- "Z:/school study/pg/fyp"
fig_dir <- file.path(root, "figures")
alpha <- 0.9
n_cal <- 96L * 14L

daily <- read_csv(file.path(root, "data/processed/aci_generation_daily_2024.csv"),
                  show_col_types = FALSE)
metrics <- read_csv(file.path(root, "data/processed/aci_generation_metrics_2024.csv"),
                    show_col_types = FALSE)

raw <- read_csv(file.path(root, "data/processed/ireland_qtr_hourly_2014_2025.csv"),
                show_col_types = FALSE)
dat <- raw %>%
  transmute(time = as.POSIXct(time, tz = "UTC"), y = generation) %>%
  arrange(time) %>%
  filter(time >= as.POSIXct("2024-01-01", tz = "UTC"),
         time < as.POSIXct("2025-01-01", tz = "UTC")) %>%
  mutate(yhat = dplyr::lag(y, 96L), residual = y - yhat) %>%
  filter(!is.na(yhat))

half <- as.numeric(quantile(abs(dat$residual[seq_len(n_cal)]), probs = alpha, na.rm = TRUE))
fixed_daily <- dat %>%
  mutate(
    covered = abs(y - yhat) <= half,
    day = as.Date(time)
  ) %>%
  filter(dplyr::row_number() > n_cal) %>%
  group_by(method = "FixedWidth", day) %>%
  summarise(coverage = mean(covered), mean_width = 2 * half, y_mean = mean(y), .groups = "drop")

daily_all <- bind_rows(daily, fixed_daily)
monthly <- daily_all %>%
  mutate(month = as.Date(format(day, "%Y-%m-01"))) %>%
  group_by(method, month) %>%
  summarise(coverage = mean(coverage), .groups = "drop")

theme_set(theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom"))

p2 <- ggplot(monthly, aes(x = month, y = coverage, colour = method)) +
  geom_hline(yintercept = alpha, linetype = "dashed", colour = "#7B241C") +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0.7, 1)) +
  scale_colour_manual(values = c(
    FixedWidth = "#B9770E",
    AgACI = "#1B4F72",
    DtACI = "#6C3483",
    `SF-OGD` = "#0E7C66"
  )) +
  labs(
    title = "Monthly empirical coverage of 90% intervals",
    subtitle = "Ireland IE Generation, 15-minute steps, 2024",
    x = NULL, y = "Coverage", colour = NULL
  )
ggsave(file.path(fig_dir, "12_aci_monthly_coverage.png"), p2, width = 11, height = 4.8, dpi = 150)

metrics_plot <- metrics %>%
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
  labs(title = "Coverage by method (target 90%)", x = NULL, y = "Coverage", fill = NULL)
ggsave(file.path(fig_dir, "13_aci_coverage_by_method.png"), p3, width = 9.5, height = 4.6, dpi = 150)

p4 <- ggplot(metrics, aes(x = reorder(method, mean_width), y = mean_width)) +
  geom_col(fill = "#1B4F72", width = 0.7) +
  coord_flip() +
  labs(
    title = "Mean interval width",
    subtitle = "Smaller is better if coverage is near the target",
    x = NULL, y = "Mean width (MW)"
  )
ggsave(file.path(fig_dir, "14_aci_mean_width.png"), p4, width = 8, height = 4.2, dpi = 150)

write_csv(monthly, file.path(root, "data/processed/aci_generation_monthly_coverage_2024.csv"))
message("Rebuilt figures 12-14")

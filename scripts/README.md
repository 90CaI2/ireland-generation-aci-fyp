# Analysis scripts (start here)

1. **`aci_generation_first_run.R`** — first AdaptiveConformal run (seasonal naive).
2. **`aci_walkforward_improved.R`** — walk-forward lagged+calendar predictor + ACI (main improvement).
3. **`aci_meeting_diagnostics.R`** — conditional coverage, path length, wind-lag check.

Supporting:

- `install_packages_r44.R` — install AdaptiveConformal stack.
- `rebuild_aci_summary_plots.R` — regenerate a subset of summary plots from saved metrics.

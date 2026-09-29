# Cutter called-strikes and whiffs

**Memo:** [What job does the cutter actually do?](https://hblaney.github.io/cutter-cs-whiff-analysis/cutter-report.html)

I looked at whether cutters get called strikes or whiffs, whether pitchers throw them in the wrong counts, and where the pitch is actually worth something.

Data is Baseball Savant regular season, 2024–2026 (`game_type = R`). Pitcher-years need 150 cutters. Run value is `delta_pitcher_run_exp` (not flipped). The memo is `cutter-report.html` (or knit `cutter-report.Rmd`).

- Called-strike rate is about 33% and whiff rate about 21%, next to four-seams. With two strikes the cutter still whiffs about 20%.
- Called-strike rate tracks zone rate. Two-strike usage already follows whiff rate. Median strike / whiff labels do not predict next year’s run value.
- Glove-side and away from same-handed hitters is the better cell. Opposite-handed and in is the worse one. Same pitcher, both versions.

## Run

Working directory = this folder.

Packages: `dplyr`, `ggplot2`, `knitr`. The pull uses `sabRmetrics` (`data_collection.R`). `prep.R` is the working notebook.

1. Run `data_collection.R` if `data/pitches_2024.rds`, `pitches_2025.rds`, and `pitches_2026.rds` are missing.  
2. Open `cutter-report.html`, or knit `cutter-report.Rmd` (needs the three `.rds` files).


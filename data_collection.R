library(sabRmetrics)

p2024 <- download_baseballsavant("2024-01-01", "2024-12-31", game_type = "R")
p2025 <- download_baseballsavant("2025-01-01", "2025-12-31", game_type = "R")
p2026 <- download_baseballsavant("2026-01-01", "2026-12-31", game_type = "R")

saveRDS(p2024, "data/pitches_2024.rds")
saveRDS(p2025, "data/pitches_2025.rds")
saveRDS(p2026, "data/pitches_2026.rds")

dplyr::arrange(game_date, game_id, at_bat_number, pitch_number)

p2024 <- p2024 |>
  dplyr::arrange(game_date, game_id, at_bat_number, pitch_number)
p2025 <- p2025 |>
  dplyr::arrange(game_date, game_id, at_bat_number, pitch_number)
p2026 <- p2026 |>
  dplyr::arrange(game_date, game_id, at_bat_number, pitch_number)

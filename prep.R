library(dplyr)
library(ggplot2)
library(baseballr)

p2024 <- readRDS("data/pitches_2024.rds")
p2025 <- readRDS("data/pitches_2025.rds")
p2026 <- readRDS("data/pitches_2026.rds")

pitches <- dplyr::bind_rows(p2024, p2025, p2026)

pitches <- pitches |>
  mutate(
    is_take = description %in% c("ball", "blocked_ball", "called_strike"),
    is_swing = description %in% c("foul", "foul_tip", "swinging_strike", "swinging_strike_blocked", "hit_into_play"),
    is_called_strike = description == ("called_strike"),
    is_whiff = description %in% c("swinging_strike", "swinging_strike_blocked")
  )

table(pitches$description[!pitches$is_take & !pitches$is_swing])

pitches <- pitches |>
  mutate(rv = delta_pitcher_run_exp)
  
pitches |>
  group_by(description) |>
  summarise(mean(rv, na.rm = TRUE))

league <- pitches |>
  filter(pitch_type %in% c("FF", "SI", "FC", "SL", "ST", "CU", "CH", "KC", "FS", "F0")) |>
  group_by(year, pitch_type) |>
  summarise(
    n = n(),
    cs_per_take = sum(is_called_strike) / sum(is_take),
    whiff_per_swing = sum(is_whiff) / sum(is_swing),
    .groups = "drop"
  )

View(league) 

ggplot(league, aes(x=year, y=cs_per_take, color = pitch_type)) +
  geom_line() +
  geom_point()
  
ggplot(league, aes(x=year, y=whiff_per_swing, color = pitch_type)) +
  geom_line() +
  geom_point()

cutters <- pitches |>
  filter(pitch_type == "FC") |>
  group_by(pitcher_id, year) |>
  summarise(
    n = n(),
    cs_per_take_pp = sum(is_called_strike) / sum(is_take),
    whiff_per_swing_pp = sum(is_whiff) / sum(is_swing),
    avg_run_value_per_100 = 100 * mean(rv, na.rm=TRUE),
    pct_two_strikes = mean(strikes == 2, na.rm=TRUE),
    pct_behind = mean(balls > strikes, na.rm=TRUE),
    .groups = "drop"
  ) |>
  filter(n >= 150)

cutters |>
  filter(avg_run_value_per_100 == max(avg_run_value_per_100))

cor(cutters$cs_per_take_pp, cutters$whiff_per_swing_pp)  

med_cs <- median(cutters$cs_per_take_pp, na.rm = TRUE)
med_wh <- median(cutters$whiff_per_swing_pp, na.rm = TRUE)

cutters <- cutters |>
  mutate(
    type = case_when(
      cs_per_take_pp >= med_cs & whiff_per_swing_pp >= med_wh ~ "both",
      cs_per_take_pp >= med_cs & whiff_per_swing_pp < med_wh ~ "strike-leaning",
      cs_per_take_pp < med_cs & whiff_per_swing_pp >= med_wh ~ "whiff-leaning",
      TRUE ~ "neither"
    )
  )

by_type <- cutters |>
  group_by(type) |>
  summarise(
    n_pitcher_years = n(),
    rv_per_100 = mean(avg_run_value_per_100, na.rm = TRUE),
    .groups = "drop"
  )

by_type

year_comp45 <- cutters |>
  filter(year == 2025) |>
  select(pitcher_id, rv_2025 = avg_run_value_per_100) |>
  inner_join(
    cutters |> filter(year == 2024) |> select(pitcher_id, type_2024 = type),
    by = "pitcher_id"
  )

year_comp45

year_comp45 |>
  group_by(type_2024) |>
  summarise(
    n = n(),
    rv_2025 = mean(rv_2025, na.rm = TRUE),
    .groups = "drop"
  )

year_comp45 <- year_comp45 |>
  inner_join(
    cutters |> filter(year == 2025) |> select(pitcher_id, type_2025 = type),
    by = "pitcher_id"
  )

year_comp56 <- cutters |>
  filter(year == 2026) |>
  select(pitcher_id, rv_2026 = avg_run_value_per_100) |>
  inner_join(
    cutters |> filter(year == 2025) |> select(pitcher_id, type_2025 = type),
    by = "pitcher_id"
  )

year_comp56

year_comp56 |>
  group_by(type_2025) |>
  summarise(
    n = n(),
    rv_2026 = mean(rv_2026, na.rm = TRUE),
    .groups = "drop"
  )

year_comp56 <- year_comp56 |>
  inner_join(
    cutters |> filter(year == 2026) |> select(pitcher_id, type_2026 = type),
    by = "pitcher_id"
  )

mean(year_comp45$type_2024 == year_comp45$type_2025)
mean(year_comp56$type_2025 == year_comp56$type_2026)

cutter_pitches <- pitches |>
  filter(pitch_type == "FC") |>
  semi_join(cutters, by = c("year", "pitcher_id")) |>
  mutate(
    situation = case_when(
      strikes == 2 ~ "two_strikes",
      balls > strikes ~ "behind",
      TRUE ~ "other"
    )
  )

cutter_pitches

usage <- cutter_pitches |>
  group_by(situation) |>
  summarise(
    n = n(),
    rv_per_100 = 100 * mean(rv, na.rm = TRUE),
    .groups = "drop"
  )

usage

baseline <- pitches |>
  mutate(
    situation = case_when(
      strikes == 2 ~ "two_strikes",
      balls > strikes ~ "behind",
      TRUE ~ "other"
    )
  ) |>
  group_by(situation) |>
  summarise(league_rv_per_100 = 100 * mean(rv, na.rm = TRUE), .groups = "drop")

usage |>
  left_join(baseline, by = "situation") |>
  mutate(cutter_vs_league = rv_per_100 - league_rv_per_100)

type_usage <- cutter_pitches |>
  inner_join(
    cutters |>
      transmute(year, pitcher_id, cutter_type = type),
    by = c("year", "pitcher_id")
  ) |>
  group_by(cutter_type, situation) |>
  summarise(
    n = n(),
    rv_per_100 = 100 * mean(rv, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(n >= 30)

type_usage

job_count <- pitches |>
  filter(pitch_type %in% c("FF", "FC", "SL")) |>
  mutate(
    situation = case_when(
      strikes == 2 ~ "two_strikes",
      balls > strikes ~ "behind",
      TRUE ~ "other"
    )
  ) |>
  group_by(pitch_type, situation) |>
  summarise(
    n = n(),
    cs_per_take = sum(is_called_strike) / sum(is_take),
    whiff_per_swing = sum(is_whiff) / sum(is_swing),
    rv_per_100 = 100 * mean(rv, na.rm = TRUE),
    .groups = "drop"
  )

job_count |>
  filter(situation %in% c("behind", "two_strikes"))

cor(cutters$cs_per_take_pp, cutters$whiff_per_swing_pp)
cor(cutters$cs_per_take_pp, cutters$pct_two_strikes)
cor(cutters$whiff_per_swing_pp, cutters$pct_two_strikes)

cutters_zone <- pitches |>
  filter(pitch_type == "FC") |>
  mutate(in_zone = zone %in% 1:9) |>
  group_by(year, pitcher_id) |>
  summarise(zone_pp = mean(in_zone, na.rm = TRUE), .groups = "drop")

cutters <- cutters |>
  left_join(cutters_zone, by = c("year", "pitcher_id"))

cor(cutters$cs_per_take_pp, cutters$zone_pp)

yoy_rates <- function(y1, y2) {
  cutters |>
    filter(year == y1) |>
    select(pitcher_id, cs1 = cs_per_take_pp, wh1 = whiff_per_swing_pp,
           rv1 = avg_run_value_per_100, k1 = pct_two_strikes) |>
    inner_join(
      cutters |>
        filter(year == y2) |>
        select(pitcher_id, cs2 = cs_per_take_pp, wh2 = whiff_per_swing_pp,
               rv2 = avg_run_value_per_100, k2 = pct_two_strikes),
      by = "pitcher_id"
    )
}

y45 <- yoy_rates(2024, 2025)
y56 <- yoy_rates(2025, 2026)

cor(y45$cs1, y45$cs2)
cor(y45$wh1, y45$wh2)
cor(y45$rv1, y45$rv2)
cor(y45$k1, y45$k2)
cor(y45$wh1, y45$k2)

cor(y56$cs1, y56$cs2)
cor(y56$wh1, y56$wh2)
cor(y56$rv1, y56$rv2)
cor(y56$k1, y56$k2)
cor(y56$wh1, y56$k2)

cutter_loc <- cutter_pitches |>
  filter(!is.na(plate_x)) |>
  mutate(
    matchup = if_else(pitch_hand == bat_side, "same", "opp"),
    glove_x = if_else(pitch_hand == "R", plate_x, -plate_x),
    side = if_else(glove_x >= 0, "glove", "arm")
  )

loc_2x2 <- cutter_loc |>
  group_by(matchup, side) |>
  summarise(
    n = n(),
    cs_per_take = sum(is_called_strike) / sum(is_take),
    whiff_per_swing = sum(is_whiff) / sum(is_swing),
    rv_per_100 = 100 * mean(rv, na.rm = TRUE),
    .groups = "drop"
  )

loc_2x2

cells <- cutter_loc |>
  group_by(year, pitcher_id, matchup, side) |>
  summarise(
    n = n(),
    cs = sum(is_called_strike) / sum(is_take),
    whiff = sum(is_whiff) / sum(is_swing),
    rv100 = 100 * mean(rv, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(n >= 25)

sg <- cells |>
  filter(matchup == "same", side == "glove") |>
  select(year, pitcher_id, rv_s = rv100, cs_s = cs, wh_s = whiff)

og <- cells |>
  filter(matchup == "opp", side == "glove") |>
  select(year, pitcher_id, rv_o = rv100, cs_o = cs, wh_o = whiff)

paired_glove <- inner_join(sg, og, by = c("year", "pitcher_id"))

nrow(paired_glove)
mean(paired_glove$rv_s)
mean(paired_glove$rv_o)
t.test(paired_glove$rv_s, paired_glove$rv_o, paired = TRUE)
t.test(paired_glove$wh_s, paired_glove$wh_o, paired = TRUE)
t.test(paired_glove$cs_s, paired_glove$cs_o, paired = TRUE)

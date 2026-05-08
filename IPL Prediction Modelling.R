install.packages(c("tidyverse", "lubridate", "janitor", "glue", "tidymodels", 
                   "xgboost", "ranger", "glmnet", "probably", "ggplot2", "scales",
                   "ggtext", "patchwork", "gt", "gtExtras", "here", "tictoc", 
                   "furrr", "vip"))

library(tidyverse)
library(lubridate)
library(janitor)
library(glue)
library(tidymodels)   # unified modelling framework
library(xgboost)      # gradient boosting
library(ranger)       # random forest package
library(glmnet)       # logistic regression
library(probably)     # probability calibration
library(ggplot2)
library(scales)
library(ggtext)
library(patchwork)
library(gt)
library(gtExtras)
library(here)
library(tictoc)
library(furrr)      # nice package to use to speed up rolling calculations
library(vip)

##################################### Setup ######################################

# Project constants
current_season <- "2026"
historical_data <- read.csv("~/Downloads/IPL.csv")
matches_2026 <- read.csv("~/Downloads/archive (7)/matches.csv")
deliveries_2026 <- read.csv("~/Downloads/archive (7)/deliveries.csv")
squads_2026 <- read.csv("~/Downloads/archive (7)/squads.csv")
venues_2026 <- read.csv("~/Downloads/archive (7)/venues.csv")
points_2026 <- read.csv("~/Downloads/archive (7)/points_table.csv")

## Team names standardisation
team_name_map <- c(
  "Delhi Daredevils" = "Delhi Capitals",
  "Kings XI Punjab" = "Punjab Kings",
  "Royal Challengers Bangalore" = "Royal Challengers Bengaluru",
  "Rising Pune Supergiant" = "Rising Pune Supergiants",
  "Deccan Chargers" = "Deccan Chargers",                # defunct (old team)
  "Kochi Tuskers Kerala" = "Kochi Tuskers Kerala",      # defunct (old team)
  "Pune Warriors" = "Pune Warriors",                    # defunct (old team)
  "Gujarat Lions" = "Gujarat Lions"                     # defunct (old team)
)

# Active 2026 teams
active_teams_2026 <- c("Chennai Super Kings", "Delhi Capitals", "Gujarat Titans",
  "Kolkata Knight Riders", "Lucknow Super Giants", "Mumbai Indians", "Punjab Kings",
  "Rajasthan Royals", "Royal Challengers Bengaluru", "Sunrisers Hyderabad")

# Colour palette for data visualisation
team_colours <- c(
  "Chennai Super Kings" = "#FDB913",
  "Delhi Capitals" = "#0078BC",
  "Gujarat Titans" = "#0C192D",
  "Kolkata Knight Riders" = "#3A225D",
  "Lucknow Super Giants" = "#7C1C1C",
  "Mumbai Indians" = "#003FA0",
  "Punjab Kings" = "#C9A227",
  "Rajasthan Royals" = "#D415DA",
  "Royal Challengers Bengaluru" = "#EC1C24",
  "Sunrisers Hyderabad" = "#FF822A"
)

##################################### Data prep ######################################
# Ensuring historical data columns are correct dtype
historical_data <- read_csv("IPL.csv",
  col_types = cols(
    match_id = col_character(),
    date = col_date(format = "%Y-%m-%d"),
    season = col_character(),
    innings = col_integer(),
    over = col_integer(),
    ball = col_integer(),
    runs_batter = col_integer(),
    runs_extras = col_integer(),
    runs_total = col_integer(),
    valid_ball = col_integer()
  )
)

### Harmonise team names
harmonise_teams <- function(x) {
  recode(x, !!!team_name_map)
}

deliveries_clean <- historical_data %>%
  mutate(
    batting_team = harmonise_teams(batting_team),
    bowling_team = harmonise_teams(bowling_team),
    toss_winner = harmonise_teams(toss_winner),
    match_won_by = harmonise_teams(match_won_by),
    season_year = {
      first_year <- as.integer(str_extract(season, "^\\d{4}"))
      is_split <- str_detect(season, "/")
      if_else(is_split, first_year + 1L, first_year)
    }
  )

# Match level table
## This match level table will help extract distinct match records for efficient modelling later.
## The historical ball-by-ball file contains match metadata repeated on every row.

matches_clean <- deliveries_clean %>%
  group_by(match_id) %>%
  slice(1) %>%              # 1 row per match
  ungroup() %>%
  select(
    match_id, date, season, season_year, venue, city, batting_team, bowling_team,
    toss_winner, toss_decision, match_won_by, win_outcome, stage, event_match_no, 
    player_of_match, balls_per_over, overs) %>%
  rename(
    team1 = batting_team,
    team2 = bowling_team) %>%
  mutate(
    bat_first = team1, # which team batted first
    bat_second = team2,
    toss_bat = toss_decision == "bat", # did toss winner choose to bat first
    winner = match_won_by,
    team1_won = as.integer(winner == team1), # binary win/no win for team 1 (useful for some models)
    has_result = !is.na(win_outcome) & win_outcome != "no result"
  )

# DQ checks
## Fixed the 2007/2008, 2009/2010, 2020/2021 seasons which are not getting picked up by the season and is coming up as NA
message("\n--- Season coverage ---")
matches_clean %>%
  count(season_year, season) %>%
  print(n = 30)

message("\n--- Null check on key modelling columns ---")
matches_clean %>%
  select(match_id, date, team1, team2, toss_winner, toss_decision, winner) %>%
  summarise(across(everything(), ~sum(is.na(.)))) |>
  print()

message("\n--- Active teams appearing in 2025/2026 ---")
matches_clean %>%
  filter(season_year >= 2025) %>%
  distinct(team1) %>%
  arrange(team1) %>%
  print()

##################################### EDA ######################################
### Helper: consistent ggplot theme for this project
theme_ipl <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_markdown(face = "bold", size = 14),
      plot.subtitle = element_text(colour = "grey40", size = 11),
      plot.caption = element_text(colour = "grey60", size = 9),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

# Season win counts
season_winners <- matches_clean %>%
  filter(has_result, !is.na(winner)) %>%
  count(winner, name = "wins") %>%
  arrange(desc(wins)) %>%
  mutate(
    winner = fct_reorder(winner, wins),
    is_active = winner %in% active_teams_2026,
    bar_colour = if_else(is_active, team_colours[as.character(winner)], "grey70")
  )

p1 <- season_winners |>
  ggplot(aes(x = winner, y = wins, fill = I(bar_colour))) +
  geom_col(width = 0.7) +
  geom_text(aes(label = wins), hjust = -0.2, size = 3.5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "**IPL all-time match wins** (2008–present)",
    subtitle = "Greyed bars = defunct teams",
    x = NULL,
    y = "Matches won",
  ) +
  theme_ipl()

ggsave("~/Desktop/plots/eda_01_all_time_wins.png", p1, width = 9, height = 6, dpi = 150)

# Toss advantage (likelihood of winning the game when toss is won)
toss_analysis <- matches_clean %>%
  filter(has_result, !is.na(winner)) %>%
  mutate(toss_winner_won = toss_winner == winner) %>%
  group_by(season_year) %>%
  summarise(
    matches = n(),
    toss_winner_won = sum(toss_winner_won),
    pct = toss_winner_won / matches
  )

p2 <- toss_analysis |>
  ggplot(aes(x = season_year, y = pct)) +
  geom_line(colour = "#004BA0", linewidth = 1) +
  geom_point(colour = "#004BA0", size = 2.5) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey50") +
  scale_y_continuous(labels = percent_format(), limits = c(0.3, 0.7)) +
  scale_x_continuous(breaks = seq(2008, 2026, 2)) +
  labs(
    title = "**Toss win → match win rate** by season",
    subtitle = "Dashed line = 50% (no toss advantage)",
    x = "Season",
    y = "% of toss winners who won the match"
  ) +
  theme_ipl()

ggsave(here("plots", "eda_02_toss_advantage.png"), p2, width = 9, height = 5, dpi = 150)

# Toss decision split (bat first vs field first over time)
toss_decision <- matches_clean %>%
  filter(!is.na(toss_decision)) %>%
  count(season_year, toss_decision) %>%
  group_by(season_year) %>%
  mutate(pct = n / sum(n))

p3 <- toss_decision %>%
  ggplot(aes(x = season_year, y = pct, fill = toss_decision)) +
  geom_area(alpha = 0.85, position = "stack") +
  scale_fill_manual(values = c("bat" = "#F7A721", "field" = "#0078BC"),
                    labels = c("bat" = "Chose to bat", "field" = "Chose to field")) +
  scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(breaks = seq(2008, 2026, 2)) +
  labs(
    title = "**Toss decision trends** — bat first vs field first",
    x = "Season",
    y = "Proportion of toss winners",
    fill = NULL
  ) +
  theme_ipl()

ggsave(here("plots", "eda_03_toss_decision.png"), p3, width = 9, height = 5, dpi = 150)

# Average 1st innings score by season
innings1_scores <- deliveries_clean %>%
  filter(innings == 1, valid_ball == 1) %>%
  group_by(match_id, season_year) %>%
  summarise(total_runs = sum(runs_total), .groups = "drop")

p4 <- innings1_scores %>%
  group_by(season_year) %>%
  summarise(
    avg_score = mean(total_runs),
    median_score = median(total_runs),
    q25 = quantile(total_runs, 0.25),
    q75 = quantile(total_runs, 0.75)
  ) %>%
  ggplot(aes(x = season_year)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = "#EC1C24", alpha = 0.15) +
  geom_line(aes(y = avg_score), colour = "#EC1C24", linewidth = 1) +
  geom_point(aes(y = avg_score), colour = "#EC1C24", size = 2.5) +
  scale_x_continuous(breaks = seq(2008, 2026, 2)) +
  labs(
    title    = "**Average 1st innings total** by season",
    subtitle = "Shaded band = interquartile range",
    x        = "Season",
    y        = "Runs scored (1st innings)"
  ) +
  theme_ipl()

ggsave(here("plots", "eda_04_scoring_trends.png"), p4, width = 9, height = 5, dpi = 150)

# Head-to-head matrix for active 2026 teams
h2h <- matches_clean %>%
  filter(
    has_result,
    team1 %in% active_teams_2026,
    team2 %in% active_teams_2026,
    !is.na(winner)
  ) %>%
  mutate(
    # Normalise so every pairing appears once per direction
    win_team1 = as.integer(winner == team1)
  ) %>%
  group_by(team1, team2) %>%
  summarise(
    matches  = n(),
    team1_wins = sum(win_team1),
    win_pct  = team1_wins / matches,
    .groups  = "drop"
  )

# Shorten team names for the matrix axis
short_names <- c(
  "Chennai Super Kings" = "CSK",
  "Delhi Capitals" = "DC",
  "Gujarat Titans" = "GT",
  "Kolkata Knight Riders" = "KKR",
  "Lucknow Super Giants" = "LSG",
  "Mumbai Indians" = "MI",
  "Punjab Kings" = "PBKS",
  "Rajasthan Royals" = "RR",
  "Royal Challengers Bengaluru" = "RCB",
  "Sunrisers Hyderabad" = "SRH"
)

# NOTE THE ABOVE H2H IS INCORRECT #

# Build base pairwise H2H table
h2h_base <- matches_clean %>%
  filter(
    has_result,
    !is.na(winner),
    team1 %in% active_teams_2026,
    team2 %in% active_teams_2026
  ) %>%
  mutate(
    team_a = pmin(team1, team2),
    team_b = pmax(team1, team2),
    team_a_won = winner == team_a
  ) %>%
  group_by(team_a, team_b) %>%
  summarise(
    matches = n(),
    team_a_wins = sum(team_a_won),
    team_a_win_pct = team_a_wins / matches,
    .groups = "drop"
  )

h2h_correct <- bind_rows(
  h2h_base %>%
    transmute(
      team1 = team_a,
      team2 = team_b,
      matches = matches,
      wins = team_a_wins,
      win_pct = team_a_win_pct
    ),
  
  h2h_base %>%
    transmute(
      team1 = team_b,
      team2 = team_a,
      matches = matches,
      wins = matches - team_a_wins,
      win_pct = 1 - team_a_win_pct
    )
)

# Heatmap (Correct version)
p5 <- h2h_correct %>%
  mutate(
    t1 = recode(team1, !!!short_names),
    t2 = recode(team2, !!!short_names)
  ) %>%
  ggplot(aes(x = t2, y = t1, fill = win_pct)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(
    aes(label = sprintf("%.0f%%\n(%d)", win_pct * 100, matches)),
    size = 2.8,
    colour = "white",
    fontface = "bold"
  ) +
  scale_fill_gradient2(
    low = "#004BA0",
    mid = "grey85",
    high = "#EC1C24",
    midpoint = 0.5,
    labels = scales::percent_format()
  ) +
  labs(
    title = "**Head-to-head win % matrix** (active teams, all time)",
    subtitle = "Each cell shows row team's win % against column team",
    x = "Opponent (column team)",
    y = "Team (row)",
    fill = "Win %"
  ) +
  theme_ipl() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("plots", "eda_05_h2h_matrix.png"), p5, width = 10, height = 8, dpi = 150)

# Home advantage analysis
# The IPL.csv venue strings are inconsistent (e.g. "M Chinnaswamy Stadium" vs
# "M. Chinnaswamy Stadium", sometimes with city appended, sometimes not).
# The most robust approach: extract the city from the venue string and join
# to venues_2026 on city, then resolve the home team from there.

venues_map <- venues_2026|>
  clean_names() |>
  # Expand: each city maps to its home team (abbreviation → full name)
  select(city, home_team) |>
  # home_team in venues_2026 is stored as abbreviation (e.g. "MI") — map to full name
  mutate(home_team_full = recode(home_team,
                                 "MI" = "Mumbai Indians",
                                 "CSK" = "Chennai Super Kings",
                                 "KKR" = "Kolkata Knight Riders",
                                 "RCB" = "Royal Challengers Bengaluru",
                                 "DC" = "Delhi Capitals",
                                 "PBKS" = "Punjab Kings",
                                 "RR" = "Rajasthan Royals",
                                 "SRH" = "Sunrisers Hyderabad",
                                 "GT" = "Gujarat Titans",
                                 "LSG" = "Lucknow Super Giants"
  )) |>
  select(city, home_team = home_team_full) |>
  # Some cities appear under multiple spellings in IPL.csv — add aliases
  bind_rows(tibble(
    city = c("Bengaluru", "Mohali", "Visakhapatnam", "Navi Mumbai",
                  "Pune", "Raipur", "Ranchi", "Cuttack"),
    home_team = c("Royal Challengers Bengaluru", "Punjab Kings", "Sunrisers Hyderabad",
                  "Mumbai Indians", "Mumbai Indians", "Chennai Super Kings",
                  "Kolkata Knight Riders", "Kolkata Knight Riders")
  ))

# Extract city from venue string: IPL.csv stores venue as
# "Stadium Name, City" or just "Stadium Name"
# The city is the last comma-separated token; if absent, use the `city` column
venue_wins <- matches_clean |>
  filter(has_result, !is.na(winner), season_year >= 2015) |>
  mutate(
    # Pull city from venue string (last token after final comma), else fall back to city col
    venue_city = coalesce(
      str_trim(str_extract(venue, "[^,]+$")),   # last segment after comma
      city
    )
  ) |>
  # Join on city name
  left_join(venues_map, by = c("venue_city" = "city")) |>
  filter(!is.na(home_team), home_team %in% c(team1, team2)) |>
  mutate(home_won = winner == home_team) |>
  group_by(home_team) |>
  summarise(
    home_matches = n(),
    home_wins = sum(home_won),
    home_win_pct = home_wins / home_matches
  ) |>
  filter(home_team %in% active_teams_2026) |>
  mutate(home_team = fct_reorder(home_team, home_win_pct))

p6 <- venue_wins |>
  ggplot(aes(x = home_team, y = home_win_pct,
             fill = I(team_colours[as.character(home_team)]))) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey40") +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.9)) +
  geom_text(aes(label = sprintf("%.0f%%", home_win_pct * 100)),
            hjust = -0.3, size = 5.5, fontface = "bold") +
  coord_flip() +
  labs(
    title = "**Home ground win %** (since 2015)",
    subtitle = "Dashed line = 50% baseline",
    x = NULL,
    y = "Home win %"
  ) +
  theme_ipl()

ggsave(here("plots", "eda_06_home_advantage.png"), p6, width = 9, height = 6, dpi = 150)

############################ Feature Engineering ###############################
# The purpose of this is to develop predictive features for the match-winner model.

# Features built include:
#   - Rolling team win rate (last 5 and last 10 matches)
#   - Exponentially weighted form (actuarial decay weighting)
#   - Head-to-head win rate (historical)
#   - Toss interaction (won toss AND field first)
#   - Venue win rate per team
#   - ELO ratings (updated after every match)
#   - Team strength index (average runs scored vs conceded, last 10)
#   - Player ratings (using performance so far in 2026 season & auction price per player)

matches_result <- matches_clean %>%
  filter(has_result, !is.na(winner)) %>%
  mutate(venue = str_trim(str_remove(venue, ",.*$"))) %>%
  # Standardise venue names (i.e. we have "Wankhede Stadium, Mumbai")
  # and "Wankhede Stadium", which are treated as 2 separate venues)
  arrange(date, match_id)

## Helper functions
### Rolling win rate - for a vector of binary win indicators (win = 1)
rolling_win_rate <- function(x, n = 5) {
  slider::slide_dbl(x, mean, .before = n, .after = 0, .complete = FALSE) %>%
    lag(1)      # use past data only (exclude the current match)
}

## Exponential decay weighting (actuarial credibility-style)
### More recent matches weighted more heavily. Lambda controls decay speed.
exp_weighted_avg <- function(x, lambda = 0.85) {
  n <- length(x)
  out <- numeric(n)
  for (i in seq_along(x)) {
    if (i == 1) {
      out[i] <- NA_real_
      next
    }
    past_vals <- x[seq_len(i-1)]
    k <- length(past_vals)
    weights <- lambda^(rev(seq_len(k)) - 1)
    out[i] <- sum(weights * past_vals) / sum(weights)
  }
  out
}

# 1. Build a LONG per-team match history
# Each match appears TWICE — once for each team — so we can compute
# team-specific rolling statistics efficiently.

team_history <- bind_rows(
  matches_result |>
    transmute(
      match_id, date, season_year,
      team      = team1,
      opponent  = team2,
      won       = as.integer(winner == team1),
      toss_won  = as.integer(toss_winner == team1),
      venue
    ),
  matches_result |>
    transmute(
      match_id, date, season_year,
      team      = team2,
      opponent  = team1,
      won       = as.integer(winner == team2),
      toss_won  = as.integer(toss_winner == team2),
      venue
    )
) |>
  arrange(date, match_id, team)

# 2. Rolling form features per team
team_features <- team_history |>
  group_by(team) |>
  arrange(date, match_id) |>
  mutate(
    # Simple rolling win rates
    win_rate_last5  = rolling_win_rate(won, n = 5),
    win_rate_last10 = rolling_win_rate(won, n = 10),
    # Exponentially weighted win rate (more actuarial — downweights old games)
    win_rate_exp    = exp_weighted_avg(won, lambda = 0.85),
    # Cumulative win rate in this season
    season_wins_so_far   = cumsum(won) - won,       # exclude current
    season_played_so_far = row_number() - 1,
    season_win_rate_ytd  = if_else(
      season_played_so_far > 0,
      season_wins_so_far / season_played_so_far,
      NA_real_
    )
  ) |>
  ungroup() |>
  select(match_id, team, win_rate_last5, win_rate_last10, win_rate_exp,
         season_win_rate_ytd)

# 3. Head-to-head win rate

h2h_rates <- team_history |>
  arrange(date, match_id) |>
  group_by(team, opponent) |>
  mutate(
    h2h_wins_so_far   = cumsum(won) - won,
    h2h_played_so_far = row_number() - 1,
    h2h_win_rate      = if_else(
      h2h_played_so_far > 0,
      h2h_wins_so_far / h2h_played_so_far,
      0.5        # shrink toward 0.5 (no prior info → assume equal)
    )
  ) |>
  ungroup() |>
  select(match_id, team, opponent, h2h_win_rate)


# 4. Venue win rate per team
venue_rates <- team_history |>
  arrange(date, match_id) |>
  group_by(team, venue) |>
  mutate(
    venue_wins_so_far   = cumsum(won) - won,
    venue_played_so_far = row_number() - 1,
    venue_win_rate      = if_else(
      venue_played_so_far >= 3,                # need at least 3 games
      venue_wins_so_far / venue_played_so_far,
      NA_real_                                 # flag: insufficient data
    )
  ) |>
  ungroup() |>
  select(match_id, team, venue_win_rate)

# 5. ELO rating system
# Classic Elo: K-factor of 32, starting rating 1500
# Updated after every match; we capture the pre-match rating as the feature
elo_update <- function(rating_a, rating_b, won_a, K = 32) {
  expected_a <- 1 / (1 + 10 ^ ((rating_b - rating_a) / 400))
  new_a      <- rating_a + K * (won_a - expected_a)
  new_b      <- rating_b + K * ((1 - won_a) - (1 - expected_a))
  list(new_a = new_a, new_b = new_b)
}

# Initialise all teams at 1500
all_teams  <- unique(c(matches_result$team1, matches_result$team2))
elo_ratings <- setNames(rep(1500, length(all_teams)), all_teams)

elo_records <- vector("list", nrow(matches_result))

for (i in seq_len(nrow(matches_result))) {
  row   <- matches_result[i, ]
  t1    <- row$team1
  t2    <- row$team2
  won_t1 <- as.integer(row$winner == t1)
  
  # Record PRE-MATCH ratings as features
  elo_records[[i]] <- tibble(
    match_id   = row$match_id,
    elo_team1  = elo_ratings[t1],
    elo_team2  = elo_ratings[t2],
    elo_diff   = elo_ratings[t1] - elo_ratings[t2]
  )
  
  # Update ratings
  new_elos          <- elo_update(elo_ratings[t1], elo_ratings[t2], won_t1)
  elo_ratings[t1]   <- new_elos$new_a
  elo_ratings[t2]   <- new_elos$new_b
}

elo_df <- bind_rows(elo_records)
message("ELO ratings computed for all matches.")

# 6. Innings scoring strength (avg runs scored & conceded, last 10 matches)
innings_strength <- deliveries_clean |>
  filter(innings %in% 1:2, valid_ball == 1) |>
  group_by(match_id, batting_team, bowling_team, innings) |>
  summarise(runs = sum(runs_total), .groups = "drop") |>
  # Join to get dates for ordering
  left_join(matches_result |> select(match_id, date), by = "match_id") |>
  arrange(date, match_id)

batting_strength <- innings_strength |>
  group_by(batting_team) |>
  arrange(date) |>
  mutate(
    avg_runs_scored_last10 = slider::slide_dbl(runs, mean, .before = 10, .after = 0) |> lag(1)
  ) |>
  ungroup() |>
  select(match_id, batting_team, avg_runs_scored_last10)

bowling_strength <- innings_strength |>
  group_by(bowling_team) |>
  arrange(date) |>
  mutate(
    avg_runs_conceded_last10 = slider::slide_dbl(runs, mean, .before = 10, .after = 0) |> lag(1)
  ) |>
  ungroup() |>
  select(match_id, bowling_team, avg_runs_conceded_last10)

# 7. Assemble model dataset — join all features onto match table
# Features for team1
team1_features <- team_features |>
  rename_with(~paste0(.x, "_t1"), -c(match_id, team)) |>
  rename(team1 = team)

team2_features <- team_features |>
  rename_with(~paste0(.x, "_t2"), -c(match_id, team)) |>
  rename(team2 = team)

# Rename opponent so we can join on both team columns — this ensures we pull
# the H2H rate for the specific team1 vs team2 matchup in each match
h2h_t1 <- h2h_rates |>
  rename(team1 = team, team2 = opponent, h2h_win_rate_t1 = h2h_win_rate)

h2h_t2 <- h2h_rates |>
  rename(team2 = team, team1 = opponent, h2h_win_rate_t2 = h2h_win_rate)

venue_t1 <- venue_rates |>
  rename(team1 = team, venue_win_rate_t1 = venue_win_rate)

venue_t2 <- venue_rates |>
  rename(team2 = team, venue_win_rate_t2 = venue_win_rate)

batting_t1 <- batting_strength |>
  rename(team1 = batting_team, avg_runs_scored_t1 = avg_runs_scored_last10)

batting_t2 <- batting_strength |>
  rename(team2 = batting_team, avg_runs_scored_t2 = avg_runs_scored_last10)

bowling_t1 <- bowling_strength |>
  rename(team1 = bowling_team, avg_runs_conceded_t1 = avg_runs_conceded_last10)

bowling_t2 <- bowling_strength |>
  rename(team2 = bowling_team, avg_runs_conceded_t2 = avg_runs_conceded_last10)

model_data <- matches_result |>
  left_join(team1_features, by = c("match_id", "team1")) |>
  left_join(team2_features, by = c("match_id", "team2")) |>
  left_join(h2h_t1 |> select(match_id, team1, team2, h2h_win_rate_t1),
            by = c("match_id", "team1", "team2")) |>
  left_join(venue_t1 |> select(match_id, team1, venue_win_rate_t1),
            by = c("match_id", "team1")) |>
  left_join(venue_t2 |> select(match_id, team2, venue_win_rate_t2),
            by = c("match_id", "team2")) |>
  left_join(batting_t1, by = c("match_id", "team1")) |>
  left_join(batting_t2, by = c("match_id", "team2")) |>
  left_join(bowling_t1, by = c("match_id", "team1")) |>
  left_join(bowling_t2, by = c("match_id", "team2")) |>
  left_join(elo_df, by = "match_id") |>
  mutate(
    # Toss interaction feature: won toss AND chose to field (historically advantageous)
    toss_field_advantage = as.integer(toss_winner == team1 & toss_decision == "field"),
    # Difference features (actuarial: relative risk is more stable than absolute)
    elo_diff             = elo_team1 - elo_team2,
    win_rate_diff_last5  = win_rate_last5_t1 - win_rate_last5_t2,
    win_rate_diff_exp    = win_rate_exp_t1 - win_rate_exp_t2,
    h2h_advantage        = h2h_win_rate_t1 - 0.5,   # centred: positive = team1 historically beats team2
    net_scoring_diff     = (avg_runs_scored_t1 - avg_runs_conceded_t2) -
      (avg_runs_scored_t2 - avg_runs_conceded_t1),
    # Outcome variable for modelling
    outcome              = factor(team1_won, levels = c(0, 1), labels = c("team2_wins", "team1_wins"))
  )

# Auction price
IPL_auction_prices <- read.csv("~/Desktop/IPL_auction_prices.csv")

# Batting rating
batting_stats <- deliveries_clean %>%
  group_by(batter) %>%
  summarise(
    runs = sum(runs_batter, na.rm = TRUE),
    balls = sum(valid_ball, na.rm = TRUE),
    dismissals = sum(striker_out == 1, na.rm = TRUE),
    avg = runs / pmax(dismissals, 1),
    strike_rate = 100 * runs / pmax(balls, 1),
    .groups = "drop"
  ) %>%
  mutate(
    # Credibility: low-ball players get shrunk heavily
    batting_credibility = pmin(balls / 300, 1),
    
    raw_batting_rating =
      0.45 * as.numeric(scale(avg)) +
      0.35 * as.numeric(scale(strike_rate)) +
      0.20 * as.numeric(scale(log1p(runs))),
    
    batting_rating = batting_credibility * raw_batting_rating
  )

# Bowling rating
bowling_stats <- deliveries_clean %>%
  group_by(bowler) %>%
  summarise(
    runs_conceded = sum(runs_bowler, na.rm = TRUE),
    balls = sum(valid_ball, na.rm = TRUE),
    overs = balls / 6,
    wickets = sum(bowler_wicket, na.rm = TRUE),
    economy = runs_conceded / pmax(overs, 1),
    bowling_avg = runs_conceded / pmax(wickets, 1),
    bowling_sr = balls / pmax(wickets, 1),
    .groups = "drop"
  ) %>%
  mutate(
    # Credibility: low-over bowlers get shrunk heavily
    bowling_credibility = pmin(overs / 50, 1),
    
    raw_bowling_rating =
      0.35 * -as.numeric(scale(economy)) +
      0.35 * -as.numeric(scale(bowling_avg)) +
      0.20 * -as.numeric(scale(bowling_sr)) +
      0.10 *  as.numeric(scale(log1p(wickets))),
    
    bowling_rating = bowling_credibility * raw_bowling_rating
  )

# Combine batting + bowling ratings into one performance rating
player_ratings <- full_join(
  batting_stats %>%
    select(player = batter, runs, balls, avg, strike_rate, batting_rating),
  bowling_stats %>%
    select(player = bowler, overs, wickets, economy, bowling_avg, bowling_sr, bowling_rating),
  by = "player"
) %>%
  mutate(
    across(c(batting_rating, bowling_rating), ~ replace_na(.x, 0)),
    
    performance_rating = batting_rating + bowling_rating
  )

# Clean auction data and combine with performance rating
auction_data <- IPL_auction_prices %>%
  rename(
    player = Players,
    sold_price = Sold
  ) %>%
  mutate(
    sold_price = as.numeric(sold_price),
    auction_rating = as.numeric(scale(log1p(sold_price)))
  )

# Final player rating = performance + auction prior
final_player_ratings <- player_ratings %>%
  left_join(
    auction_data %>% select(player, sold_price, auction_rating),
    by = "player"
  ) %>%
  mutate(
    sold_price = replace_na(sold_price, 0),
    auction_rating = replace_na(auction_rating, 0),
    final_rating = 0.7 * performance_rating + 0.3 * auction_rating
  ) %>%
  arrange(desc(final_rating))

# Team strength
squads_2026 <- squads_2026 %>%
  mutate(
    team_name = recode(team_name,
                       "Gujrat Titans" = "Gujarat Titans")
  )

team_strength <- squads_2026 %>%
  left_join(final_player_ratings, by = "player") %>%
  group_by(team_name) %>%
  summarise(
    squad_strength = sum(final_rating, na.rm = TRUE),
    avg_strength = mean(final_rating, na.rm = TRUE),
    n_players = n(),
    .groups = "drop"
  )

################################## Modelling ######################################
# The 3 models that are going to be fit and compared are:
# 1. logistic regression (GLM baseline)
# 2. Random forest (captures non-linear relationships)
# 3. XGBoost (gradient boosting)

# Validation strategy: walk-forward time split (respect time order)

# Build 2026 squad/team strength from final_player_ratings
squad_strength_2026 <- squads_2026 %>%
  clean_names() %>%
  rename(
    team = team_name,
    player = player
  ) %>%
  mutate(
    team = recode(team,
                  "Gujrat Titans" = "Gujarat Titans",
                  "Royal Challengers Bangalore" = "Royal Challengers Bengaluru",
                  "Kings XI Punjab" = "Punjab Kings"
    )
  ) %>%
  left_join(final_player_ratings, by = "player") %>%
  group_by(team) %>%
  arrange(desc(final_rating)) %>%
  slice_head(n = 15) %>%
  summarise(
    squad_strength = sum(final_rating, na.rm = TRUE),
    avg_player_strength = mean(final_rating, na.rm = TRUE),
    n_players_used = n(),
    .groups = "drop"
  )

# Add team strength into model_data
team_strength_t1 <- squad_strength_2026 %>%
  rename(
    team1 = team,
    strength_t1 = squad_strength,
    avg_strength_t1 = avg_player_strength
  )

team_strength_t2 <- squad_strength_2026 %>%
  rename(
    team2 = team,
    strength_t2 = squad_strength,
    avg_strength_t2 = avg_player_strength
  )

model_data <- model_data %>%
  # Remove ANY old strength columns first
  select(-matches("strength"), -matches("n_players_used")) %>%
  
  # Join team1 strength
  left_join(
    team_strength_t1 %>%
      select(team1, strength_t1 = strength_t1, avg_strength_t1 = avg_strength_t1),
    by = "team1"
  ) %>%
  
  # Join team2 strength
  left_join(
    team_strength_t2 %>%
      select(team2, strength_t2 = strength_t2, avg_strength_t2 = avg_strength_t2),
    by = "team2"
  ) %>%
    mutate(
    strength_diff = strength_t1 - strength_t2,
    avg_strength_diff = avg_strength_t1 - avg_strength_t2
  )

## Define feature set
feature_cols <- c(
  # ELO-based
  "elo_diff",
  
  # Player/squad strength
  "strength_diff",
  "avg_strength_diff",
  
  # Rolling form
  "win_rate_last5_t1", "win_rate_last5_t2",
  "win_rate_diff_last5",
  "win_rate_exp_t1", "win_rate_exp_t2",
  "win_rate_diff_exp",
  
  # Head-to-head
  "h2h_win_rate_t1", "h2h_advantage",
  
  # Venue
  "venue_win_rate_t1", "venue_win_rate_t2",
  
  # Scoring strength
  "avg_runs_scored_t1", "avg_runs_scored_t2",
  "avg_runs_conceded_t1", "avg_runs_conceded_t2",
  "net_scoring_diff",
  
  # Toss
  "toss_field_advantage",
  
  # Season context
  "season_win_rate_ytd_t1", "season_win_rate_ytd_t2"
)

model_input <- model_data %>%
  filter(season_year >= 2011) %>%
  select(all_of(c("match_id", "date", "season_year", feature_cols, "outcome"))) %>%
  drop_na(outcome, elo_diff)

# Walk-forward time split
## Train on seasons up to 2023, test on 2024 + 2025
split_year <- 2023

train_data <- model_input %>%
  filter(season_year <= split_year)

test_data <- model_input %>%
  filter(season_year > split_year & season_year < 2026)

# Recipe (preprocessing)
## Tidymodels recipe = declare transformations once, apply to train and test

ipl_recipe <- recipe(outcome ~ ., data = train_data) |>
  update_role(match_id, date, season_year, new_role = "ID") |>
  step_zv(all_predictors()) |>             # remove zero-variance FIRST
  step_nzv(all_predictors()) |>            # also remove near-zero-variance
  step_impute_median(all_numeric_predictors()) |>    # fill NAs with median
  step_normalize(all_numeric_predictors())           # z-score normalise last

# Models specifications
## Logistic regression
logit_spec <- logistic_reg(
  penalty = tune(), # L1/L2 via glmnet
  mixture = 1) %>%  # LASSO: forces uninformative features to zero
  set_engine("glmnet") %>%
  set_mode("classification")

## Random forest
rf_spec <- rand_forest(
  mtry = tune(),
  trees = 500,
  min_n = tune()) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

## XGBoost
xgb_spec <- boost_tree(
  trees = 500,
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  sample_size = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# Workflows
logit_wf <- workflow() %>%
  add_recipe(ipl_recipe) %>%
  add_model(logit_spec)

rf_wf <- workflow() %>%
  add_recipe(ipl_recipe) %>% 
  add_model(rf_spec)

xgb_wf <- workflow() %>% 
  add_recipe(ipl_recipe) %>% 
  add_model(xgb_spec)

# Cross-validation (5-fold on training data)
set.seed(12345)

cv_folds <- vfold_cv(train_data, v = 5, strata = outcome)
cv_metrics <- metric_set(roc_auc, mn_log_loss, accuracy, brier_class)

# Tune each model
tic("Logistic regression tuning")
logit_tune <- tune_grid(
  logit_wf,
  resamples = cv_folds,
  grid      = grid_regular(penalty(), levels = 20),
  metrics   = cv_metrics
)
toc()

tic("Random forest tuning")
rf_tune <- tune_grid(
  rf_wf,
  resamples = cv_folds,
  grid      = grid_latin_hypercube(
    mtry(range = c(3L, length(feature_cols))),
    min_n(),
    size = 15
  ),
  metrics = cv_metrics
)
toc()

tic("XGBoost tuning")
xgb_tune <- tune_grid(
  xgb_wf,
  resamples = cv_folds,
  grid      = grid_latin_hypercube(
    tree_depth(), learn_rate(), loss_reduction(), sample_prop(),
    size = 20
  ),
  metrics = cv_metrics
)
toc()

# Select best hyperparameters and fit final models
best_logit <- select_best(logit_tune, metric = "mn_log_loss")
best_rf    <- select_best(rf_tune,    metric = "mn_log_loss")
best_xgb   <- select_best(xgb_tune,  metric = "mn_log_loss")

final_logit <- finalize_workflow(logit_wf, best_logit) |> fit(train_data)
final_rf    <- finalize_workflow(rf_wf,    best_rf)    |> fit(train_data)
final_xgb   <- finalize_workflow(xgb_wf,  best_xgb)   |> fit(train_data)

# Evaluate on held-out test set
evaluate_model <- function(model, test, model_name) {
  preds <- augment(model, new_data = test)
  cv_metrics(
    preds,
    truth        = outcome,
    estimate     = .pred_class,
    .pred_team2_wins,          # first factor level — required by prob metrics
    event_level  = "first"
  ) |>
    mutate(model = model_name)
}

test_results <- bind_rows(
  evaluate_model(final_logit, test_data, "Logistic (GLM)"),
  evaluate_model(final_rf,    test_data, "Random Forest"),
  evaluate_model(final_xgb,   test_data, "XGBoost")
)

message("\n--- Test set performance ---")
test_results |>
  select(model, .metric, .estimate) |>
  pivot_wider(names_from = .metric, values_from = .estimate) |>
  arrange(mn_log_loss) |>
  print()

# Feature importance (from best model)
xgb_importance <- final_xgb |>
  extract_fit_parsnip() |>
  vip::vi() |>
  mutate(Variable = fct_reorder(Variable, Importance))

p_importance <- xgb_importance |>
  head(15) |>
  ggplot(aes(x = Variable, y = Importance)) +
  geom_col(fill = "#004BA0", width = 0.7) +
  coord_flip() +
  labs(
    title    = "XGBoost feature importance (top 15)",
    subtitle = "Gain-based importance",
    x        = NULL,
    y        = "Importance (gain)"
  ) +
  theme_minimal(base_size = 12)

ggsave(here("plots", "model_feature_importance.png"),
       p_importance, width = 9, height = 6, dpi = 150)

############################### Final Prediction ################################
# Current ELO ratings using all played matches up to now
elo_final <- model_data %>%
  filter(has_result, !is.na(winner)) %>%
  arrange(date, match_id) %>%
  (\(df) {
    
    all_teams <- unique(c(df$team1, df$team2))
    elo_ratings <- setNames(rep(1500, length(all_teams)), all_teams)
    K <- 32
    
    walk(seq_len(nrow(df)), function(i) {
      row <- df[i, ]
      
      t1 <- row$team1
      t2 <- row$team2
      w  <- as.integer(row$winner == t1)
      
      ea <- 1 / (1 + 10 ^ ((elo_ratings[t2] - elo_ratings[t1]) / 400))
      
      elo_ratings[t1] <<- elo_ratings[t1] + K * (w - ea)
      elo_ratings[t2] <<- elo_ratings[t2] + K * ((1 - w) - (1 - ea))
    })
    
    tibble(
      team = names(elo_ratings),
      elo  = unname(elo_ratings)
    )
  })()

# Latest team form features
latest_features <- model_data %>%
  filter(!is.na(win_rate_exp_t1)) %>%
  arrange(date) %>%
  {
    bind_rows(
      select(., match_id, date, team = team1,
             win_rate_last5  = win_rate_last5_t1,
             win_rate_last10 = win_rate_last10_t1,
             win_rate_exp    = win_rate_exp_t1,
             venue_win_rate  = venue_win_rate_t1,
             avg_runs_scored = avg_runs_scored_t1,
             avg_runs_conceded = avg_runs_conceded_t1,
             season_win_rate_ytd = season_win_rate_ytd_t1),
      
      select(., match_id, date, team = team2,
             win_rate_last5  = win_rate_last5_t2,
             win_rate_last10 = win_rate_last10_t2,
             win_rate_exp    = win_rate_exp_t2,
             venue_win_rate  = venue_win_rate_t2,
             avg_runs_scored = avg_runs_scored_t2,
             avg_runs_conceded = avg_runs_conceded_t2,
             season_win_rate_ytd = season_win_rate_ytd_t2)
    )
  } %>%
  group_by(team) %>%
  slice_max(date, n = 1, with_ties = FALSE) %>%
  ungroup()

# Read remaining fixtures
remaining_fixtures <- read.csv("~/Desktop/IPL_upcoming_fixture.csv") %>%
  mutate(
    match_id = as.character(match_id),
    season_year = as.integer(season_year),
    date = as.Date(date)
  )

# Prepare latest features for team1/team2
features_t1 <- latest_features %>%
  rename_with(~ paste0(.x, "_t1"), -team) %>%
  rename(team1 = team)

features_t2 <- latest_features %>%
  rename_with(~ paste0(.x, "_t2"), -team) %>%
  rename(team2 = team)

# Prepare squad strength for team1/team2
strength_pred_t1 <- squad_strength_2026 %>%
  rename(
    team1 = team,
    strength_t1 = squad_strength,
    avg_strength_t1 = avg_player_strength
  )

strength_pred_t2 <- squad_strength_2026 %>%
  rename(
    team2 = team,
    strength_t2 = squad_strength,
    avg_strength_t2 = avg_player_strength
  )

# Convert fixtures into model-ready rows
pred_rows <- remaining_fixtures %>%
  left_join(features_t1, by = "team1") %>%
  left_join(features_t2, by = "team2") %>%
  left_join(elo_final %>% rename(team1 = team, elo_team1 = elo), by = "team1") %>%
  left_join(elo_final %>% rename(team2 = team, elo_team2 = elo), by = "team2") %>%
  left_join(strength_pred_t1, by = "team1") %>%
  left_join(strength_pred_t2, by = "team2") %>%
  mutate(
    elo_diff = elo_team1 - elo_team2,
    strength_diff = strength_t1 - strength_t2,
    avg_strength_diff = avg_strength_t1 - avg_strength_t2,
    win_rate_diff_last5 = win_rate_last5_t1 - win_rate_last5_t2,
    win_rate_diff_exp = win_rate_exp_t1 - win_rate_exp_t2,
    h2h_win_rate_t1 = 0.5,
    h2h_advantage = 0,
    net_scoring_diff = (avg_runs_scored_t1 - avg_runs_conceded_t2) -
      (avg_runs_scored_t2 - avg_runs_conceded_t1),
    toss_field_advantage = 0,
    season_win_rate_ytd_t1 = season_win_rate_ytd_t1,
    season_win_rate_ytd_t2 = season_win_rate_ytd_t2,
    outcome = factor(NA, levels = levels(train_data$outcome))
  )

# Predict probabilities
pred_probs <- predict(final_xgb, new_data = pred_rows, type = "prob") %>%
  bind_cols(pred_rows %>% select(match_id, date, team1, team2, venue)) %>%
  select(match_id, date, team1, team2, venue, .pred_team1_wins, .pred_team2_wins) %>%
  rename(
    prob_team1 = .pred_team1_wins,
    prob_team2 = .pred_team2_wins
  )

# Monte Carlo winner probability simulation
N_SIMS <- 10000
set.seed(123)

# Current points table
current_points <- points_2026 %>%
  transmute(
    team = team,
    points = points,
    nrr = nrr,
    played = matches
  )

# Helper: simulate one match from model probability
simulate_match <- function(team1, team2, prob_team1) {
  if (runif(1) < prob_team1) team1 else team2
}

# Helper: get model probability for any playoff match
predict_playoff_prob <- function(team1_name, team2_name, venue_name = "Neutral") {
  
  playoff_fixture <- tibble(
    match_id = paste0("playoff_", sample(1e9, 1)),
    date = Sys.Date(),
    season_year = 2026L,
    team1 = team1_name,
    team2 = team2_name,
    venue = venue_name
  )
  
  playoff_row <- playoff_fixture %>%
    left_join(features_t1, by = "team1") %>%
    left_join(features_t2, by = "team2") %>%
    left_join(elo_final %>% rename(team1 = team, elo_team1 = elo), by = "team1") %>%
    left_join(elo_final %>% rename(team2 = team, elo_team2 = elo), by = "team2") %>%
    left_join(strength_pred_t1, by = "team1") %>%
    left_join(strength_pred_t2, by = "team2") %>%
    mutate(
      elo_diff = elo_team1 - elo_team2,
      strength_diff = strength_t1 - strength_t2,
      avg_strength_diff = avg_strength_t1 - avg_strength_t2,
      win_rate_diff_last5 = win_rate_last5_t1 - win_rate_last5_t2,
      win_rate_diff_exp = win_rate_exp_t1 - win_rate_exp_t2,
      h2h_win_rate_t1 = 0.5,
      h2h_advantage = 0,
      net_scoring_diff = (avg_runs_scored_t1 - avg_runs_conceded_t2) -
        (avg_runs_scored_t2 - avg_runs_conceded_t1),
      toss_field_advantage = 0,
      outcome = factor(NA, levels = levels(train_data$outcome))
    )
  
  predict(final_xgb, new_data = playoff_row, type = "prob") %>%
    pull(.pred_team1_wins)
}

# Prepare remaining match probabilities
remaining_match_probs <- pred_probs %>%
  select(match_id, date, team1, team2, prob_team1, prob_team2)

# Run simulations
sim_results <- map_dfr(seq_len(N_SIMS), function(sim) {
  
  # Start from current points
  pts <- current_points %>%
    select(team, points) %>%
    deframe()
  
  nrr_lookup <- current_points %>%
    select(team, nrr) %>%
    deframe()
  
  # Simulate remaining league matches
  for (i in seq_len(nrow(remaining_match_probs))) {
    
    game <- remaining_match_probs[i, ]
    
    winner <- simulate_match(
      team1 = game$team1,
      team2 = game$team2,
      prob_team1 = game$prob_team1
    )
    
    pts[winner] <- pts[winner] + 2
  }
  
  # Build final simulated league table
  final_table <- tibble(
    team = names(pts),
    points = as.numeric(pts),
    nrr = as.numeric(nrr_lookup[names(pts)])
  ) %>%
    arrange(desc(points), desc(nrr))
  
  top4 <- final_table %>%
    slice_head(n = 4) %>%
    pull(team)
  
  # -------------------------
  # Page Playoff System
  # -------------------------
  
  # Qualifier 1: 1st vs 2nd
  q1_prob <- predict_playoff_prob(top4[1], top4[2])
  q1_winner <- simulate_match(top4[1], top4[2], q1_prob)
  q1_loser <- setdiff(top4[1:2], q1_winner)
  
  # Eliminator: 3rd vs 4th
  elim_prob <- predict_playoff_prob(top4[3], top4[4])
  elim_winner <- simulate_match(top4[3], top4[4], elim_prob)
  
  # Qualifier 2: loser of Q1 vs winner of Eliminator
  q2_prob <- predict_playoff_prob(q1_loser, elim_winner)
  q2_winner <- simulate_match(q1_loser, elim_winner, q2_prob)
  
  # Final: winner of Q1 vs winner of Q2
  final_prob <- predict_playoff_prob(q1_winner, q2_winner)
  champion <- simulate_match(q1_winner, q2_winner, final_prob)
  
  tibble(
    sim = sim,
    champion = champion,
    top1 = top4[1],
    top2 = top4[2],
    top3 = top4[3],
    top4 = top4[4]
  )
})

# Title probabilities
title_probs <- sim_results %>%
  count(champion, name = "titles") %>%
  mutate(
    title_prob = titles / N_SIMS,
    team_colour = team_colours[champion]
  ) %>%
  arrange(desc(title_prob))

message("\n--- 2026 IPL Title Probabilities (Monte Carlo) ---")

title_probs %>%
  mutate(title_prob = percent(title_prob, accuracy = 0.1)) %>%
  select(champion, title_prob) %>%
  print()

# Top 4 probabilities
top4_probs <- sim_results %>%
  pivot_longer(cols = c(top1, top2, top3, top4),
               names_to = "position",
               values_to = "team") %>%
  count(team, name = "top4_finishes") %>%
  mutate(top4_prob = top4_finishes / N_SIMS) %>%
  arrange(desc(top4_prob))

message("\n--- 2026 IPL Top 4 Probabilities ---")

top4_probs %>%
  mutate(top4_prob = scales::percent(top4_prob, accuracy = 0.1)) %>%
  select(team, top4_prob) %>%
  print()


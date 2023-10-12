source("src/get-player-data.R")
source("src/get-team-data.R")
source("src/get-hero-data.R")
source("src/get-match-data.R")
source("src/calc-exp-summary.R")

library(tidyverse)
library(googlesheets4)

# Get data
league_id <- 15728
players <- get_player_data(league_id)
teams <- get_team_data(league_id)
heroes <- get_hero_data() %>% select(hero_id, hero_name)

match_ids <- get_match_ids(league_id)
match_ids_blacklist <- scan(
  "data/matches/match_ids_blacklist.csv", 
  quiet = TRUE
)
match_ids <- setdiff(match_ids, match_ids_blacklist)

matches_odota <- get_match_odota_data(match_ids)
matches_replay <- get_match_replay_data(match_ids)

# Calculate fantasy points
fantasy_points <- data.frame(
  player_id = as.numeric(),
  time = as.numeric(),
  emblem_colour = as.character(),
  emblem_stat = as.character(),
  points = as.numeric()
)

i <- 1
for (match_id in match_ids) {
  print(i)
  odota_data <- matches_odota[[as.character(match_id)]]
  replay_data <- right_join(
    heroes,
    matches_replay[[as.character(match_id)]],
    by = "hero_name"
  ) %>%
    distinct()
  
  duration <- odota_data$objectives[[length(odota_data$objectives)]]$time
  neutrals <- data.frame(
    player_id = as.numeric(),
    is_radiant = as.logical(),
    kills = as.numeric()
  )
  for (player in odota_data$players) {
    neutrals <- neutrals %>%
      add_row(
        player_id = player$account_id,
        is_radiant = player$isRadiant,
        kills = player$neutral_kills
      )
  }
  neutrals <- neutrals %>%
    group_by(is_radiant) %>%
    mutate(perc_kills = kills / sum(kills)) %>%
    ungroup() %>%
    mutate(
      tokens_available = if (duration > 63*60) {
        25
      } else if (duration > 40*60) {
        20
      } else if (duration > 30*60) {
        15
      } else if (duration > 20*60) {
        10
      } else if (duration > 10*60) {
        5
      } else {
        0
      },
      tokens = perc_kills * tokens_available
    ) %>%
    select(player_id, tokens)
  
  
  for (player in odota_data$players) {
    if (player$account_id %in% players$player_id) {
      base_row <- list2(
        player_id = player$account_id,
        time = odota_data$start_time,
      )
      
      ## Kills
      fantasy_points <- fantasy_points %>% 
        add_row(
          !!!base_row,
          emblem_colour = "Red",
          emblem_stat = "Kills",
          points = player$kills * 125
        ) 
      
      ## Deaths
      fantasy_points <- fantasy_points %>% 
        add_row(
          !!!base_row,
          emblem_colour = "Red",
          emblem_stat = "Deaths",
          points = 2600 - (player$deaths * 260)
        )
      
      ## Creep Score
      fantasy_points <- fantasy_points %>% 
        add_row(
          !!!base_row,
          emblem_colour = "Red",
          emblem_stat = "Creep Score",
          points = (player$last_hits + player$denies) * 3
        )
      
      ## GPM
      fantasy_points <- fantasy_points %>% 
        add_row(
          !!!base_row,
          emblem_colour = "Red",
          emblem_stat = "GPM",
          points = player$gold_per_min * 2
        )
      
      ## Neutral Tokens Found
      fantasy_points <- fantasy_points %>%
        add_row(
          !!!base_row,
          emblem_colour = "Red",
          emblem_stat = "Neutral Tokens Found",
          points = filter(neutrals, player_id == player$account_id)$tokens * 350
        )
      
      ## Tower Kills
      fantasy_points <- fantasy_points %>% 
        add_row(
          !!!base_row,
          emblem_colour = "Red",
          emblem_stat = "Tower Kills",
          points = player$towers_killed * 325
        )
      
      ## Wards Placed
      fantasy_points <- fantasy_points %>% 
        add_row(
          !!!base_row,
          emblem_colour = "Blue",
          emblem_stat = "Wards Placed",
          points = player$obs_placed * 145
        )
      
      ## Camps Stacked
      fantasy_points <- fantasy_points %>% 
        add_row(
          !!!base_row,
          emblem_colour = "Blue",
          emblem_stat = "Camps Stacked",
          points = player$camps_stacked * 225
        )
      
      ## Runes Grabbed
      fantasy_points <- fantasy_points %>% 
        add_row(
          !!!base_row,
          emblem_colour = "Blue",
          emblem_stat = "Runes Grabbed",
          points = player$rune_pickups * 105
        )
      
      ## Watchers Taken
      fantasy_points <- fantasy_points %>%
        add_row(
          !!!base_row,
          emblem_colour = "Blue",
          emblem_stat = "Watchers Taken",
          points = replay_data %>%
            filter(
              hero_id == player$hero_id, 
              mod_name == "modifier_lamp_on"
            ) %>%
            nrow() * 235
        )
      
      ## Smokes Used
      fantasy_points <- fantasy_points %>% 
        add_row(
          !!!base_row,
          emblem_colour = "Blue",
          emblem_stat = "Smokes Used",
          points = sum(player$item_uses$smoke_of_deceit) * 390
        )
      
      ## Lotuses Grabbed
      fantasy_points <- fantasy_points %>%
        add_row(
          !!!base_row,
          emblem_colour = "Blue",
          emblem_stat = "Lotuses Grabbed",
          points = replay_data %>%
            filter(
              hero_id == player$hero_id, 
              mod_name == "modifier_pluck_famango_channel"
            ) %>%
            arrange(mod_time) %>%
            mutate(
              mod_duration = as.integer(mod_time - lag(mod_time, n = 1))
            ) %>%
            filter(mod_type == "remove") %>%
            pull(mod_duration) %>%
            sum() * 240
        )
      
      ## Roshan Kills
      fantasy_points <- fantasy_points %>% 
        add_row(
          !!!base_row,
          emblem_colour = "Green",
          emblem_stat = "Roshan Kills",
          points = player$roshans_killed * 890
        )
      
      ## Teamfight Participation
      fantasy_points <- fantasy_points %>% 
        add_row(
          !!!base_row,
          emblem_colour = "Green",
          emblem_stat = "Teamfight Participation",
          points = player$teamfight_participation * 1835
        )
      
      ## Stuns
      fantasy_points <- fantasy_points %>% 
        add_row(
          !!!base_row,
          emblem_colour = "Green",
          emblem_stat = "Stuns",
          points = player$stuns * 20
        )
      
      ## Tormentor Kills
      fantasy_points <- fantasy_points %>% 
        add_row(
          !!!base_row,
          emblem_colour = "Green",
          emblem_stat = "Tormentor Kills",
          points = if(is.null(player$killed$npc_dota_miniboss)) {
            0 
          } else {
            player$killed$npc_dota_miniboss * 875
          }
        )
      
      # First Blood
      fantasy_points <- fantasy_points %>% 
        add_row(
          !!!base_row,
          emblem_colour = "Green",
          emblem_stat = "First Blood",
          points = player$firstblood_claimed * 2000
        )
      
      ## Courier Kills
      fantasy_points <- fantasy_points %>% 
        add_row(
          !!!base_row,
          emblem_colour = "Green",
          emblem_stat = "Courier Kills",
          points = player$courier_kills * 855
        )
    }
  }
  
  i <- i + 1
  rm(player, match_id, odota_data, replay_data, duration, neutrals, base_row)
}

# Calculate player-wise top stats
fantasy_sums <- players %>% 
  select(player_id, player_role) %>%
  inner_join(fantasy_points, by = "player_id") %>%
  group_by(player_id, player_role, emblem_colour, emblem_stat) %>%
  arrange(time) %>%
  summarise(
    average = calc_exp_summary(points, func = "average"),
    stddev = calc_exp_summary(points, func = "stddev"),
    .groups = "drop"
  )

write_csv(x = fantasy_sums, file = "results/all_stats.csv")

fantasy_sums %>%
  left_join(players, by = c("player_id", "player_role")) %>%
  left_join(teams, by = "team_id") %>%
  pivot_wider(
    id_cols = c(player_name, team_name, player_role),
    names_from = emblem_stat, 
    values_from = average,
    names_sort = TRUE
  ) %>%
  arrange(team_name, player_role, player_name) %>%
  rename(
    "Player Name" = "player_name", 
    "Team Name" = "team_name",
    "Role" = "player_role"
  ) %>%
  write_sheet(
    ss = "1QR5klw-qN5lTDqPS38LQPF5LKRs-x6ClOCzno6LdPic", 
    sheet = "Emblem Stat Data (Avg)"
  )

fantasy_sums %>%
  left_join(players, by = c("player_id", "player_role")) %>%
  left_join(teams, by = "team_id") %>%
  pivot_wider(
    id_cols = c(player_name, team_name, player_role),
    names_from = emblem_stat, 
    values_from = stddev,
    names_sort = TRUE
  ) %>%
  arrange(team_name, player_role, player_name) %>%
  rename(
    "Player Name" = "player_name", 
    "Team Name" = "team_name",
    "Role" = "player_role"
  ) %>%
  write_sheet(
    ss = "1QR5klw-qN5lTDqPS38LQPF5LKRs-x6ClOCzno6LdPic", 
    sheet = "Emblem Stat Data (Std)"
  )

# Calculate top players
top_cores <- bind_rows(
  fantasy_sums %>% 
    filter(player_role == "Core", emblem_colour == "Red") %>%
    group_by(player_id) %>%
    slice_max(order_by = average, n = 2) %>%
    ungroup(),
  fantasy_sums %>% 
    filter(player_role == "Core", emblem_colour == "Green") %>%
    group_by(player_id) %>%
    slice_max(order_by = average, n = 1) %>%
    ungroup()
) %>%
  group_by(player_id, player_role) %>%
  summarise(
    average = sum(average),
    stddev = sqrt(sum(stddev^2)),
    .groups = "drop"
  ) %>%
  arrange(desc(average))

top_mids <- bind_rows(
  fantasy_sums %>% 
    filter(player_role == "Mid", emblem_colour == "Red") %>%
    group_by(player_id) %>%
    slice_max(order_by = average, n = 1) %>%
    ungroup(),
  fantasy_sums %>% 
    filter(player_role == "Mid", emblem_colour == "Blue") %>%
    group_by(player_id) %>%
    slice_max(order_by = average, n = 1) %>%
    ungroup(),
  fantasy_sums %>% 
    filter(player_role == "Mid", emblem_colour == "Green") %>%
    group_by(player_id) %>%
    slice_max(order_by = average, n = 1) %>%
    ungroup()
) %>%
  group_by(player_id, player_role) %>%
  summarise(
    average = sum(average),
    stddev = sqrt(sum(stddev^2)),
    .groups = "drop"
  ) %>%
  arrange(desc(average))

top_supps <- bind_rows(
  fantasy_sums %>% 
    filter(player_role == "Support", emblem_colour == "Blue") %>%
    group_by(player_id) %>%
    slice_max(order_by = average, n = 2) %>%
    ungroup(),
  fantasy_sums %>% 
    filter(player_role == "Support", emblem_colour == "Green") %>%
    group_by(player_id) %>%
    slice_max(order_by = average, n = 1) %>%
    ungroup()
) %>%
  group_by(player_id, player_role) %>%
  summarise(
    average = sum(average),
    stddev = sqrt(sum(stddev^2)),
    .groups = "drop"
  ) %>%
  arrange(desc(average))

top_players <- bind_rows(
  top_cores %>% head(10),
  top_mids %>% head(5),
  top_supps %>% head(10)
)

write_csv(
  x = top_players %>% select(player_id),
  file = "data/top_players.csv",
  col_names = FALSE
)
write_csv(
  x = top_players %>%
    left_join(players, by = c("player_id", "player_role")) %>%
    left_join(teams, by = "team_id") %>%
    select(player_name, team_name, player_role, average, stddev),
  file = "results/top_players.csv"
)

# Calculate top stats
top_stats <- fantasy_sums %>%
  group_by(player_role, emblem_colour, emblem_stat) %>%
  summarise(
    all_player_average = mean(average), 
    all_player_stddev = mean(stddev), 
    .groups = "drop"
  ) %>%
  left_join(
    fantasy_sums %>%
      filter(player_id %in% (top_players %>% pull(player_id))) %>%
      group_by(player_role, emblem_colour, emblem_stat) %>%
      summarise(
        top_player_average = mean(average), 
        top_player_stddev = mean(stddev), 
        .groups = "drop"
      ),
    by = c("player_role", "emblem_colour", "emblem_stat")
  ) %>%
  filter(
    (player_role == "Core" & emblem_colour %in% c("Red", "Green")) |
      player_role == "Mid" |
      (player_role == "Support" & emblem_colour %in% c("Blue", "Green"))
  ) %>%
  arrange(player_role, emblem_colour, desc(top_player_average))

write_csv(x = top_stats, file = "results/top_stats.csv")

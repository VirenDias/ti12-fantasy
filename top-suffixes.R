source("src/get-player-data.R")
source("src/get-team-data.R")
source("src/get-match-data.R")
source("src/get-item-data.R")
source("src/calc-exp-summary.R")

library(tidyverse)
library(googlesheets4)

# Get data
league_id <- 15728
teams_elim <- c(7391077, 8244493, 8254400, 8894818)
players <- get_player_data(league_id) %>% filter(!(team_id %in% teams_elim))
teams <- get_team_data(league_id)
top_players <- scan("data/top_players.csv", quiet = TRUE)
items <- get_item_data()
suffixes <- read_csv("data/suffixes.csv", show_col_types = FALSE)

match_ids <- get_match_ids(league_id)
match_ids_blacklist <- scan(
  "data/matches/match_ids_blacklist.csv", 
  quiet = TRUE
)
match_ids <- setdiff(match_ids, match_ids_blacklist)
matches_odota <- get_match_odota_data(match_ids)

# Calculate suffix incidences
suffix_incids <- data.frame(
  player_id = as.numeric(),
  suffix_name = as.character(),
  time = as.numeric(),
  cond = as.logical()
)

i <- 1
for (match in matches_odota) {
  print(i)
  for (player in match$players) {
    if (player$account_id %in% players$player_id) {
      base_row <- list2(
        player_id = player$account_id,
        time = match$start_time,
      )
      
      # the Accomplice
      ## +10% in games where the player has the most assists
      suffix_incids <- suffix_incids %>% 
        add_row(
          !!!base_row,
          suffix_name = "the Accomplice",
          cond = sapply(
            X = match$players, 
            FUN = function(x) { 
              if (!is.null(x$assists)) x$assists else 0 
            }
          ) %>%
            max() == player$assists
        )
      
      # of the Ant
      ## +16% in games where the player has the lowest networth
      suffix_incids <- suffix_incids %>% 
        add_row(
          !!!base_row,
          suffix_name = "of the Ant",
          cond = sapply(
            X = match$players, 
            FUN = function(x) { 
              if (!is.null(x$net_worth)) x$net_worth else Inf
            }
          ) %>%
            min() == player$net_worth
        )
      
      # of the Bull
      ## +20% in games where the player buys back before 30 minutes
      suffix_incids <- suffix_incids %>% 
        add_row(
          !!!base_row,
          suffix_name = "of the Bull",
          cond = if (player$buyback_count == 0) {
            FALSE
          } else {
            sapply(X = player$buyback_log, FUN = function(x) x$time < 1800) %>%
              sum() > 0
          }
        )
      
      # the Decisive
      ## +20% in games that last less than 25 minutes
      suffix_incids <- suffix_incids %>% 
        add_row(
          !!!base_row,
          suffix_name = "the Decisive",
          cond = match$objectives[[length(match$objectives)]]$time <= 1500
        )
      
      # the Divine Thief
      ## +25% in games where any player steals a Divine Rapier
      ## Check if any player has rapier in inventory and purchase_rapier = 0 or null
      suffix_incids <- suffix_incids %>% 
        add_row(
          !!!base_row,
          suffix_name = "the Divine Thief",
          cond = sapply(
            X = match$players, 
            FUN = function(x) { 
              (x$item_0 == 133 | x$item_1 == 133 | x$item_2 == 133 | 
                x$item_3 == 133 | x$item_4 == 133 | x$item_5 == 133) &
                is.null(x$purchase_rapier)
            }
          ) %>%
            sum() > 0
        )
      
      # the Even-Keeled
      ## +20% in games where the player is tipped 5 or more times
      
      # the Flayed Twins Acolyte
      ## +15% if any player gets first blood before the starting horn
      suffix_incids <- suffix_incids %>% 
        add_row(
          !!!base_row,
          suffix_name = "the Flayed Twins Acolyte",
          cond = match$first_blood_time <= 0
        )
      
      # the Loquacious
      ## +10% in games where the player uses the most voice lines
      
      # the Mule
      ## +20% in games where the player ends the game with items in every slot 
      ## of their inventory and backpack
      suffix_incids <- suffix_incids %>% 
        add_row(
          !!!base_row,
          suffix_name = "the Mule",
          cond = player$item_0 != 0 & player$item_1 != 0 & player$item_2 != 0 &
            player$item_3 != 0 & player$item_4 != 0 & player$item_5 != 0 &
            player$backpack_0 != 0 & player$backpack_1 != 0 & 
            player$backpack_2 != 0
        )
      
      # the Nothl Pilgrim
      ## +16% if the player has the most deaths in the game
      suffix_incids <- suffix_incids %>% 
        add_row(
          !!!base_row,
          suffix_name = "the Nothl Pilgrim",
          cond = sapply(
            X = match$players, 
            FUN = function(x) { 
              if (!is.null(x$deaths)) x$deaths else 0 
            }
          ) %>%
            max() == player$deaths
        )
      
      # of the Octopus
      ## +15% in games where the player ends the game with 4 or more items that  
      ## have an active ability
      suffix_incids <- suffix_incids %>% 
        add_row(
          !!!base_row,
          suffix_name = "of the Octopus",
          cond = c(
            player$item_0, 
            player$item_1, 
            player$item_2, 
            player$item_3,
            player$item_4,
            player$item_5,
            player$item_neutral,
            player$backpack_0,
            player$backpack_1,
            player$backpack_2
          ) %in%
            (items %>% filter(has_active == TRUE) %>% pull(item_id)) %>%
            sum() >= 4
        )
      
      # the Pacifist
      ## +16% if the player ends the game with no kills
      suffix_incids <- suffix_incids %>% 
        add_row(
          !!!base_row,
          suffix_name = "the Pacifist",
          cond = player$kills == 0
        )
      
      # the Patient
      ## +20% if first blood does not happen until after 10 minutes
      suffix_incids <- suffix_incids %>% 
        add_row(
          !!!base_row,
          suffix_name = "the Patient",
          cond = match$first_blood_time > 600
        )
      
      # of the Raven
      ## +25% if any player gets a rampage
      suffix_incids <- suffix_incids %>% 
        add_row(
          !!!base_row,
          suffix_name = "of the Raven",
          cond = sapply(
            X = match$players, 
            FUN = function(x) { 
              if (!is.null(x$multi_kills[["5"]])) x$multi_kills[["5"]] else 0 
            }
          ) %>%
            sum() > 0
        )
      
      # the Tormented
      ## +25% if any player dies to a Tormentor
      suffix_incids <- suffix_incids %>% 
        add_row(
          !!!base_row,
          suffix_name = "the Tormented",
          cond = sapply(
            X = match$players, 
            FUN = function(x) { 
              if (!is.null(x$killed_by$npc_dota_miniboss)) {
                x$killed_by$npc_dota_miniboss
              } else {
                0
              }
            }
          ) %>%
            sum() > 0
        )
      
      # the Underdog
      ## +15% in games where the player loses
      suffix_incids <- suffix_incids %>% 
        add_row(
          !!!base_row,
          suffix_name = "the Underdog",
          cond = player$lose == 1
        )
    }
  }
  
  i <- i + 1
  rm(match, player, base_row)
}

# Calculate player-wise top suffixes
suffix_probs <- players %>% 
  select(player_id, player_role) %>%
  inner_join(suffix_incids, by = "player_id") %>%
  group_by(player_id, player_role, suffix_name) %>%
  arrange(time) %>%
  summarise(suffix_prob = calc_exp_summary(cond), .groups = "drop")

suffix_sums <- suffix_probs %>%
  left_join(suffixes, by = "suffix_name") %>%
  mutate(effective_bonus = (suffix_prob * suffix_bonus) / 100) %>%
  select(
    player_id, 
    player_role, 
    suffix_name, 
    suffix_bonus,
    suffix_prob,
    effective_bonus
  )

write_csv(x = suffix_sums, file = "results/all_suffixes.csv")

suffix_sums %>%
  left_join(players, by = c("player_id", "player_role")) %>%
  left_join(teams, by = "team_id") %>%
  pivot_wider(
    id_cols = c(player_name, team_name, player_role),
    names_from = suffix_name, 
    values_from = effective_bonus,
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
    sheet = "Title Suffix Data"
  )

# Calculate top suffixes
top_suffixes <- suffix_probs %>%
  group_by(player_role, suffix_name) %>%
  summarise(all_player_prob = mean(suffix_prob), .groups = "drop") %>%
  left_join(
    suffix_probs %>%
      filter(player_id %in% top_players) %>%
      group_by(player_role, suffix_name) %>%
      summarise(top_player_prob = mean(suffix_prob), .groups = "drop"),
    by = c("player_role", "suffix_name")
  ) %>%
  left_join(suffixes, by = "suffix_name") %>%
  mutate(
    all_player_bonus = all_player_prob * suffix_bonus,
    top_player_bonus = top_player_prob * suffix_bonus
  ) %>%
  select(
    player_role, 
    suffix_name, 
    suffix_desc, 
    all_player_prob,
    all_player_bonus,
    top_player_prob,
    top_player_bonus
  ) %>%
  arrange(player_role, desc(top_player_bonus))

write_csv(x = top_suffixes, file = "results/top_suffixes.csv")

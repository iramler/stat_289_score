play_by_play <- 
  play_by_play |>
  # this pack adds score to timeout rows
  fill(score) |>
  fill(home_team_score) |>
  fill(visiting_team_score)

# find winner
last_point <- play_by_play |>
    group_by(url, set) |>
    slice_tail(n=1)
last_point <-
  last_point |> 
  ungroup() |>
  mutate(winner = ifelse(home_team_score > visiting_team_score, 
                         "Home", "Away")) |>
  select(url, set, winner)





# add winner info to play by play
pbp <- left_join(play_by_play, last_point, 
          join_by(url == url, set == set) 
          )

## fill down the scores
#pbp2 <- pbp |>
#  fill(score) |>
#  fill(home_team_score) |>
#  fill(visiting_team_score)

# find timeouts
touts <- pbp |>
  filter(str_detect(play_descripton, "Timeout"))

# add row number by group
touts <- touts %>% 
  group_by(url, set) %>%
  mutate(id = row_number())

# determine first and last timeouts
touts2 <- touts |>
  group_by(url, set) %>%
  mutate(first_to = (id == min(id)),
         last_to = (id == max(id)),
         ) |>
  select(score, play_descripton, 
         home_team_score, visiting_team_score,
         set, url, 
         winner, id,
         first_to, last_to
         )

first_timeout <- touts2 %>% filter(first_to)
last_timeout <- touts2 %>% filter(last_to)

# determine who took time out
first_timeout <- first_timeout |>
    mutate(timeout_by = ifelse(home_team_score < visiting_team_score, 
                           "Home", "Away")) 

last_timeout <- last_timeout |>
  mutate(timeout_by = ifelse(home_team_score < visiting_team_score, 
                             "Home", "Away")) 

first_timeout %>%
  group_by(timeout_by,winner) |>
  tally()
(295 + 237) / (295 + 1035 +1214+237)


last_timeout %>%
  group_by(timeout_by,winner) |>
  tally()
(159 + 63) / (159 + 1209 +1350+63)

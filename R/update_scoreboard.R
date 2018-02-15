calc_elo <- function(t1_p1_elo, t1_p2_elo, t2_p1_elo, t2_p2_elo, t1_goals, t2_goals) {
    perf_team1 <-
      (2 * t1_goals * 800 - 2 * t2_goals * 800 + (t1_goals + t2_goals) * (t2_p1_elo + t2_p2_elo)) / (t1_goals +
                                                                                                       t2_goals) / 2
    perf_team2 <-
      (2 * t2_goals * 800 - 2 * t1_goals * 800 + (t1_goals + t2_goals) * (t1_p1_elo + t1_p2_elo)) / (t1_goals +
                                                                                                       t2_goals) / 2
    delta11 <- (perf_team1 - t1_p1_elo) / 30
    delta12 <- (perf_team1 - t1_p2_elo) / 30
    delta21 <- (perf_team2 - t2_p1_elo) / 30
    delta22 <- (perf_team2 - t2_p2_elo) / 30
    if (t1_goals > t2_goals) {
      print('team1 won')
      delta11 <- delta11 + 1
      delta12 <- delta12 + 1
      delta21 <- delta21 + 0.5
      delta22 <- delta22 + 0.5
    } else{
      print('team2 won')
      delta11 <- delta11 + 0.5
      delta12 <- delta12 + 0.5
      delta21 <- delta21 + 1
      delta22 <- delta22 + 1
    }
    return(
      list(
        t1_p1_elo + delta11,
        t1_p2_elo + delta12,
        t2_p1_elo + delta21,
        t2_p2_elo + delta22
      )
    )
  }

`%+=%` = function(e1, e2)
  eval.parent(substitute(e1 <- e1 + e2))

update_scoreboard <- function(sheet_url) {
  
  # Authenticate if needed
  gs_auth()
  scoreboard_sheet <- gs_url(sheet_url)
  
  # Load and reshape data
  games <- gs_read(scoreboard_sheet, 1)
  
  players <-
    gs_read(scoreboard_sheet, 3, col_names = F) %>%
    rename(name = X1) %>%
    mutate(
      score = 1500,
      games = 0,
      goals_for = 0,
      goals_against = 0
    ) %>%
    as.data.frame()
  
  rownames(players) <- players$name
  
  for (row in 1:nrow(games)) {
    game <- games[row,]
    
    new_scores <-
      calc_elo(players[game$lost_player_1, "score"],
               players[game$lost_player_2, "score"],
               players[game$won_player_1, "score"],
               players[game$won_player_2, "score"],
               game$lost_score,
               game$won_score)
    
    players[game$lost_player_1, "score"] <-
      new_scores[[1]]
    players[game$lost_player_2, "score"] <-
      new_scores[[2]]
    players[game$won_player_1, "score"] <-
      new_scores[[3]]
    players[game$won_player_2, "score"] <-
      new_scores[[4]]
    
    players[game$lost_player_1, "games"] %+=% 1
    players[game$lost_player_2, "games"] %+=% 1
    players[game$won_player_1, "games"] %+=% 1
    players[game$won_player_2, "games"] %+=% 1
    
    players[game$lost_player_1, "goals_for"] %+=% game$lost_score
    players[game$lost_player_2, "goals_for"] %+=% game$lost_score
    players[game$won_player_1, "goals_for"] %+=% game$won_score
    players[game$won_player_2, "goals_for"] %+=% game$won_score
    
    players[game$lost_player_1, "goals_against"] %+=% game$won_score
    players[game$lost_player_2, "goals_against"] %+=% game$won_score
    players[game$won_player_1, "goals_against"] %+=% game$lost_score
    players[game$won_player_2, "goals_against"] %+=% game$lost_score
  }
  
  players <- players %>%
    mutate(score = round(score),
           goals_ratio = round(goals_for / goals_against, 3)) %>%
    arrange(-score)
  
  # Save and upload the refreshed scoreboard
  gs_edit_cells(
    scoreboard_sheet,
    ws = 3,
    input = players,
    anchor = "A1",
    byrow = FALSE,
    col_names = NULL,
    trim = FALSE,
    verbose = TRUE
  )
}
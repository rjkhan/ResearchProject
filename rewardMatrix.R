rm(list = ls())
library(dplyr)


time_interval <- 120
load("data/count_matrix_120_after.Rdata")

count_group_homeresult <- function(count_matrix, time_interval, groupby = c("goaldiff", "manpowerdiff", "both")) {
  if (groupby == "manpowerdiff") {
    count <- count_matrix %>%
      group_by(interval, HomeResult, ManpowerDiff) %>%
      summarise(count = sum(count))
  }
  
  # group by interval, result and goal difference, then sum up counts
  if (groupby == "goaldiff") {
    count <- count_matrix %>%
      group_by(interval, HomeResult, GoalDiff) %>%
      summarise(count = sum(count))
  }
  
  if (groupby == "both") {
    count <- count_matrix %>%
      group_by(interval, HomeResult, ManpowerDiff, GoalDiff) %>%
      summarise(count = sum(count))
  }
  # count$interval <- as.character(count$interval)
  # lose
  count_lose <- count[which(count$HomeResult == "lose"), ]
  # win
  count_win <- count[which(count$HomeResult == "win"), ]
  # tied win
  count_tied_lose <- count[which(count$HomeResult == "tied_lose"), ]
  # tied win
  count_tied_win <- count[which(count$HomeResult == "tied_win"), ]
  
  return(list(count = count, count_lose = count_lose, count_win = count_win,
              count_tied_lose = count_tied_lose, count_tied_win = count_tied_win))
}

data = count_matrix_120_after
CountMatrix <- count_group_homeresult(data, time_interval, groupby = "both")



rewardMatrix = function(CountMatrix)
{
  ### sum up all counts in same interval goaDiff and ManpowerDiff  GD, MD at interval time t
  all <- CountMatrix$count %>% group_by(interval, ManpowerDiff, GoalDiff) %>%
          summarise(count = sum(count))
  names(all)[4] <- "all"
  
  # #in state goalDiff -1 
  data <- all
  data$GoalDiff <- data$GoalDiff - 1
  names(data)[4] <- "all_minus"
  
  
  # win and tie-win
  win <- CountMatrix$count_win
  tied_win <- CountMatrix$count_tied_win
  
  names(win)[5] <- "win_count"
  win$HomeResult <- NULL
  names(tied_win)[5] <- "tied_win_count"
  tied_win$HomeResult <- NULL
  
  wins <- full_join(win, tied_win)
  rm(win, tied_win)
  wins[is.na(wins)] <- 0
  # #(win) when <GD, MD> at interval t + #(tied win) when <GD, MD> at interval t
  wins$count <- wins$win_count + wins$tied_win_count
  wins$win_count <- NULL
  wins$tied_win_count <- NULL
  names(wins)[4] <- "count_wins"
  
  wins_minus <- wins
  wins_minus$GoalDiff <- wins_minus$GoalDiff - 1
  names(wins_minus)[4] <- "count_wins_minus"
  
  wins <- full_join(wins, wins_minus)
  wins[is.na(wins)] <- 0
  
  wins <- full_join(wins, all)
  wins <- full_join(wins, data)
  wins[is.na(wins)] <- 0
  
  wins$p <- wins$count_wins/wins$all
  wins$p_minus <- wins$count_wins_minus/wins$all_minus
  wins[is.na(wins)] <- 0
  # P(win|intervalt,GD,MD)
  wins$p_win <- wins$p - wins$p_minus
  wins <- wins[, c(1:3, 10)]
  
  
  # tied 
  # #(tied lose) when <GD, MD> at interval t
  tied <- CountMatrix$count_tied_lose
  tied$HomeResult <- NULL
  names(tied)[4] <- "count_tied"
  
  # #(tied lose) when <GD-1, MD> at interval t
  tied_minus <- tied
  tied_minus$GoalDiff <- tied_minus$GoalDiff - 1
  names(tied_minus)[4] <- "count_tied_minus"
  
  tied <- full_join(tied, tied_minus)
  tied <- full_join(tied, all)
  tied <- full_join(tied, data)
  tied[is.na(tied)] <- 0
  
  tied$p <- tied$count_tied/tied$all
  tied$p_minus <- tied$count_tied_minus/tied$all_minus
  # P(tie|intervalt,GD,MD)
  tied$p_tied <- tied$p - tied$p_minus
  tied[is.na(tied)] <- 0
  tied <- tied[, c(1:3, 10)]
  
  reward <- full_join(wins, tied)
  reward$p <- reward$p_win*2 - reward$p_tied
  reward <- reward[, c(1:3, 6)]
  reward <- reward[order(reward$interval, reward$GoalDiff), ]
  return(reward)
  
}
result  = rewardMatrix(CountMatrix = CountMatrix)
head(result)

result[order(result$p,decreasing = TRUE),]

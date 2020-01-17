rm(list = ls())

library(data.table)
library(purrr) ## is empty function

######### Split Time Interval Start and End ##############
splitInterval = function(df)
{
  
  timeInterval <- strsplit(as.character(df$interval), ",")
  
  startTime <- unlist(lapply(timeInterval, function(interval) {
    as.integer(substr(interval[1], 2, nchar(interval[1])))
  }))
  
  
  endTime <- unlist(lapply(timeInterval, function(interval) {
    as.integer(substr(interval[2], 1, nchar(interval[2]) -1))
  }))
  return(list("startTime"=startTime,"endTime"=endTime))
}

###########################################################


calculateCountMatrix <- function(df,manpwer, time_interval, method,intervalType) {
  
  ## time interval can be scalar (Seconds) value with IntervalType = 'Fixed'
  ## time interval can be vector (Seconds) value with IntervalType = 'manual'
  
  if(intervalType == "Fixed"){
    interval <- cut(df$seconds, seq(0, 20*60*3, time_interval))
    df$interval <- interval
  }
  else if(intervalType == "manual")
  {
    flag1 = ifelse(time_interval[1]==0,TRUE,FALSE)
    flag2 = ifelse(time_interval[length(time_interval)]==3600,TRUE,FALSE)
    if(!flag1)
    {
      tempV = time_interval
      tempV = c(0,tempV[1:length(tempV)])
      time_interval = tempV
    }
    if(!flag2)
    {
      tempV = time_interval
      tempV = c(tempV[1:length(tempV)],3600)
      time_interval = tempV
    }
    
    interval <- cut(df$seconds, time_interval)
    df$interval <- interval
  }
  
  ## Interval index 
  df$index <- as.integer(df$interval)
  ## default count = 1
  df$count <- 1
  
  # get the count for every time interval for each game
  count <- data.frame(GameId = as.character(), SeasonType = as.character(), 
                      interval = as.character(),
                      GF = as.integer(), GA = as.integer(), 
                      count = as.numeric(), 
                      manPowerAway = as.integer(), 
                      manPowerHome = as.integer(),
                      fAwayTeamGoals = as.integer(), 
                      fHomeTeamGoals = as.integer(), 
                      outcome = as.character())
  
  interval_levels <- factor(levels(interval), levels = levels(interval))
  n_interval <- length(levels(interval))
  
  # select the columns we need 
  tempData <- df %>%
    select(GameId, SeasonType, interval, GF, GA, count ,
           manPowerAway, manPowerHome, fAwayTeamGoals, fHomeTeamGoals, outcome)
  
  tempData <- cbind(tempData, n = 1:nrow(tempData))
  if (method == "after") {
    tempData <- tempData %>% 
      group_by(GameId, count, interval) %>%
      filter(n == max(n))
  } 
  
  if (method == "before") {
    tempData <- tempData %>% 
      group_by(GameId, count, interval) %>%
      filter(n == min(n))
  }
  tempData$n <- NULL 
  
  game_ids <- unique(tempData$GameId)
  
  for (id in game_ids) {
    gameData <- manpower[manpower$GameId == id, ]
    #the same gameId
    t <- as.data.frame(tempData[tempData$GameId == id, ])
    ## number of rows Goal Scored
    n <- nrow(t)
    # time interval numeric index
    timeIntervalIndex <- as.integer(t$interval)[1] 
    # frquency of number of goal scored in that time interval
    freq <- data.frame(table(t$interval))
    AwayGoals <- t$fAwayTeamGoals[1]
    HomeGoals <- t$fHomeTeamGoals[1]
    HomeResult <- t$outcome[1]
    SeasonType <- as.character(t$SeasonType[1])
    
    
    noGoalIndex <- which(freq$Freq == 0)
    for (i in noGoalIndex) 
    {
      # manpower numbers are same as the last event
      nn <- which((gameData$seconds < as.integer(interval_levels[i])*time_interval)  == TRUE)
      manpower_row <- nn[length(nn)]
      manpower_away = gameData[manpower_row, "manPowerAway"]
      manpower_away <- ifelse(is_empty(manpower_away),0,as.numeric(manpower_away))
      
      manpower_home <- gameData[manpower_row, "manPowerHome"]
      manpower_home <- ifelse(is_empty(manpower_home),0,as.numeric(manpower_home))
      
      if (i < timeIntervalIndex) 
      {
        dataRow =  data.frame("GameId" = id, 
                              "SeasonType" = SeasonType, 
                              "interval" = interval_levels[i],
                              "GF" = 0, 
                              "GA" = 0, 
                              "count" = 1.0,
                              "manPowerAway" = manpower_away, 
                              "manPowerHome" = manpower_home,
                              "fAwayTeamGoals" = AwayGoals, 
                              "fHomeTeamGoals" = HomeGoals, 
                              "outcome" = HomeResult)
      } 
      else 
      {
        ii <- which(t$interval == interval_levels[i-1])
        ii <- ii[length(ii)]
        dataRow =  data.frame("GameId" = id, 
                              "SeasonType" = SeasonType, 
                              "interval" = interval_levels[i],
                              "GF" = t[ii, ]$GF,
                              "GA" = t[ii, ]$GA, 
                              "count" = 1.0, 
                              "manPowerAway" = manpower_away, 
                              "manPowerHome" = manpower_home,
                              "fAwayTeamGoals" = AwayGoals, 
                              "fHomeTeamGoals" = HomeGoals,
                              "outcome" = HomeResult)
      }
      ## append row to data
      t <- rbind(t,dataRow)
    }
    
    
    t <- t[order(t$interval, t$GF, t$GA), ]
    l <- list(count, t)
    count <- rbindlist(l)
  }
  
  ## group by data to compute count
  countMatrix <- count %>%
    group_by(interval, SeasonType, GF, GA,
             manPowerAway, manPowerHome, fAwayTeamGoals, fHomeTeamGoals, outcome) %>%
    summarise(count = sum(count))
  
  # add begin time and end time columns
  r = splitInterval(countMatrix)
  ##  spilt Intrerval start
  countMatrix$startInterval = ifelse(is.na(r$startTime),0,r$startTime)
  ##  spilt Intrerval end
  countMatrix$endInterval = ifelse(is.na(r$endTime),0,r$endTime)
  
  # computing ManPower Difference 
  countMatrix$ManpowerDiff <- countMatrix$manPowerHome - countMatrix$manPowerAway
  
  # find PeriodNumber of record
  countMatrix$Period <- ifelse(countMatrix$endInterval <= 1200, "1", 
                               ifelse(countMatrix$endInterval <= 2400, "2", "3"))
  
  ## Goal Difference calulate
  countMatrix$GoalDiff <- countMatrix$GF - countMatrix$GA
  
  return(countMatrix)
}

## this 
load("data/manpower.Rdata")

## Df that we have generated from 
load('data/df.Rdata')

time_interval = c(0,100,400,800,1500,3600)

countMatrix <- calculateCountMatrix(df = df,manpwer = manpower , time_interval = time_interval,
                                    method = "before",intervalType = "manual")


calculateResult <- function(countMatrix, time_interval, type) {

  ## type define c('GoalDiff','manPowerDiff )
  if (type == "GoalDiff") {

    count <- countMatrix %>%
      group_by(interval, outcome, GoalDiff) %>%
      summarise(count = sum(count))

  }

  else if (type == "manPowerDiff") {
    count <- countMatrix %>%
      group_by(interval, outcome, ManpowerDiff) %>%
      summarise(count = sum(count))
  }

  else {
    count <- countMatrix %>%
      group_by(interval, outcome, ManpowerDiff, GoalDiff) %>%
      summarise(count = sum(count))
  }

  countWin = count[count$outcome == "win", ]
  countLose =  count[count$outcome == "lost", ]
  countTieLose =  count[count$outcome == "tie-lose", ]
  countTieWin =  count[count$outcome == "tie-win", ]


  return(list("countLose" = countLose, "countWin"=countWin,
              "countTieLose"=countTieLose,"countTieWin" = countTieWin,
              "countMatrix" = count ))
}
# type define c('GoalDiff','manPowerDiff )
r = calculateResult(countMatrix = countMatrix ,time_interval = time_interval ,
                    type  = "GoalDiff")





########################## Count Plot ##########################################################

count_plot <- function(count,time_interval, groupby, diff, result, type = c("equal", "greaterThan", "lessThan"),plotType ) {
  # labes for x axis
  labs <- levels(count$interval)
  i <- seq(round(length(labs)/length(labs), 0), length(labs), round(length(labs)/length(labs), 0))
  labs <- labs[i]
  
  "Type (equal)"
  # filter goal diff rows
  if (type == "equal") {
    count <- count[count[,groupby] == diff,]
    main_title <- paste0(result, ", ", groupby, " = ", diff)
  } else if (type == "greaterThan") {
    count <- count[count[,groupby] >= diff,]
    count <- count %>% 
      group_by(interval) %>%
      summarise(count = sum(count))
    main_title <- paste0(result, ", ", groupby, " >= ", diff)
  } else {
    count <- count[count[,groupby] <= diff,]
    count <- count %>% 
      group_by(interval) %>%
      summarise(count = sum(count))
    main_title <- paste0(result, ", ", groupby, " <= ", diff)
  } 
  
  ## find ratio in count
  count$count <- count$count/sum(count$count)*100
  
  
  ### Plot results
  par(mgp = c(0, 0.5, 0))
  plot(count$interval, count$count, xaxt="n", 
       main = main_title)
  lines(count$interval, count$count)
  abline(v = 3600/(3*time_interval) + 0.5, col = "grey", lty=3)
  abline(v = 3600*2/(3*time_interval) + 0.5, col = "grey", lty=3)
  abline(h = mean(count$count), col = "red", lty=2)
  title(xlab="Time interval (0,100,400,800,1500,3600)", ylab="cumulative count (%)", line=2, cex.lab=1.2)
  text(x=i, y=min(count$count) - (max(count$count)- min(count$count))/10,
       labels=labs, xpd=TRUE, srt=30, cex=0.5)
}
result <- "win"
diff = 1

time_interval = 120
count_plot(r$countWin,time_interval, groupby =  "GoalDiff", diff, result,"equal","normalPlot")

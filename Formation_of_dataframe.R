rm(list = ls())
library(lubridate)
library(data.table)

### load Play by Play sequence data
data = read.csv("data/pp_events.csv")

### Load goal events data
event_goal = read.csv("data/event_goal.csv")

#filter out un used columns
goal_events = event_goal[,c("GameId", "AwayTeamId", 
                            "HomeTeamId", "PeriodNumber", 
                            "EventTime", "ScoringTeamId","GoalId")]


## GameIds of season 2013-2014 and its goalKepperIds
goalKepperData = read.csv("data/golieid.csv")

## total number of players in Game
total_player =  9 


###### Find Scoring ID
findScoringTeamId = function (data)
{
  r= c()
  for (i in 1:nrow(data)) {
    
    if(data[i,"EventType"]=="GOAL"){
      index = which(event_goal$GoalId == data[i,"ExternalEventId"])
      r[i] = event_goal$ScoringTeamId[index]
    }
    else
      r[i] = NA
  }
  return(r)
}

r = findScoringTeamId(data)
data$ScoringTeamId = r


####### get away team goals
data$AwayTeamGoals <- ifelse( (data$ScoringTeamId == data$AwayTeamId) & data$EventType == "GOAL", 1, 0)
####### get home team goals
data$HomeTeamGoals <- ifelse((data$ScoringTeamId  == data$HomeTeamId) & data$EventType == "GOAL", 1, 0)


## Data Table
df <- data.table(data)


## Counting GF and GA
GF <- df[, list(GF = cumsum(AwayTeamGoals)), by=list(GameId)]
GA <- df[, list(GA = cumsum(HomeTeamGoals)), by=list(GameId)]
df <- cbind(df, GF[, -1])
df <- cbind(df, GA[, -1])



## convert EventTime into seconds
seconds<- as.numeric(hms(df$EventTime))
df$seconds = seconds



#### Manpower calculate Home and away teams and exclude the goal kepper
manPower = function(data,goalKepperData)
{
  manPowerAway = c()
  manPowerHome = c()
  for (i in 1:nrow(data)) {
    goalKepperIds = goalKepperData[goalKepperData$GameId == data[i,"GameId"],"GoalieId"]
    numberOfGoalKepperIdsMatch = length(which(data[i,11:19] %in% goalKepperIds))
    manPowerAway[i] = total_player -  length(which(is.na(data[i,11:19]))) - numberOfGoalKepperIdsMatch
    numberOfGoalKepperIdsMatch = length(which(data[i,20:28] %in% goalKepperIds))
    manPowerHome[i] = total_player -  length(which(is.na(data[i,20:28]))) - numberOfGoalKepperIdsMatch
    
  }
  return(list("manPowerHome"=manPowerHome,"manPowerAway"=manPowerAway))
}

result = manPower(df,goalKepperData)
df$manPowerAway = result$manPowerAway
df$manPowerHome = result$manPowerHome


## man power Difference 
df$ManPowerDiff = df$manPowerHome - df$manPowerAway



######## FInd Outcome ############################
findOutCome =  function(data)
{
  outcome = c()
  outcome_result = c()
  
  game_ids = unique(data$GameId)
  for (i in 1:length(game_ids)) {
    
    r = data[ data$GameId == game_ids[i],]
    numberOfGameRecord =  nrow(r)
    
    result =  r[numberOfGameRecord,]
    if( result$PeriodNumber > 3 & result$GF > result$GA ){
      outcome_result = "tie-win"
    }
    else if( result$PeriodNumber > 3 & result$GA > result$GF )
    {
      outcome_result = "tie-loss"
    }
    else if ( result$PeriodNumber == 3 & result$GF > result$GA)
    {
      outcome_result = "win"
    }
    else if ( result$PeriodNumber == 3 & result$GF < result$GA)
    {
      outcome_result = "loss"
    }
    
    
    outcome = c(outcome,rep(outcome_result,numberOfGameRecord))
  }
  return(outcome)
}


r = findOutCome(df)
df$outcome  = r



### convert time into and count additive result
seconds_to_minutes<- function(seconds){
  paste(
    formatC(seconds %/% (60*60) %% 24, width = 2, format = "d", flag = "0"),
    formatC(seconds %/% 60 %% 60, width = 2, format = "d", flag = "0"),
    formatC(seconds %% 60, width = 2, format = "d", flag = "0"),sep = ":"
  )
}




getTotalGameTime = function(data)
{
  game_ids = unique(data$GameId)
  addativeTimeInSeconds = 0
  result = c()
  minutes = c()
  
  index = 1
  for (i in game_ids) 
  {
    addativeTimeInSeconds = 0
    r = data[ data$GameId == i,]
    
    for (j in 1:nrow(r)) 
    {
      if(r[j,"PeriodNumber"]== 1 )
      {
        result[index] = r[j,"seconds"]
      }
      else
      {
        result[index] = ((r[j,"PeriodNumber"] - 1) * 20 * 60) + r[j,"seconds"] 
      }
      
      minutes[index] = seconds_to_minutes(result[index])
      index = index + 1
    }
    
  }  
  return(list("seconds" = result,"minutes" = minutes))
}
data$seconds = df$seconds
l = getTotalGameTime(data)

df$addSeconds = l$seconds
df$addEventTime = l$minutes


#################### Final GOALs of game ###################
gameEndGoals = function(df)
{
  AwayTeamGoals = rep(0,nrow(df))
  HomeTeamGoals = rep(0,nrow(df))
  
  game_ids  = unique(df$GameId)
  tempdf = df
  tempdf$index = c(1:nrow(df))
  for (id in game_ids) {
    
    gameData = tempdf[tempdf$GameId == id,]
    AwayTeamGoals[gameData$index] = sum(gameData$AwayTeamGoals)
    HomeTeamGoals[gameData$index] = sum(gameData$HomeTeamGoals)
  }
  
  return(list("fAwayTeamGoals"=AwayTeamGoals,"fHomeTeamGoals"=HomeTeamGoals))
  
}

r = gameEndGoals(df)
df$fAwayTeamGoals = r$fAwayTeamGoals
df$fHomeTeamGoals = r$fHomeTeamGoals

#############GOAL Diff ###################
## fHomeTeamGoals represent final game result of homeTeamGame
## fAwayTeamGoals represent final game result of awayTeamGame
df$goalDiff = df$fHomeTeamGoals - df$fAwayTeamGoals




############## Create Time Interval 
create_interval <- function(time_interval,data)
{
  
  game_ids = unique(data$GameId)
  interval = c()
  int = data.frame()
  for(i in game_ids)
  {
    game_data = data[ data$GameId == i,]
    maxPeriodNumber = max(game_data$PeriodNumber)
    tInterval = cut(game_data$addSeconds, seq(0,20*60*3,time_interval))
    d = data.frame(tInterval)
    int = rbind(int,d)
  }
  data$interval = int$tInterval
  return(data)
}

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

############### count = 1 for start of interval
method1 = function(df){
  
  start = unique(df$startInterval)
  end =  unique(df$endInterval)
  
  tempDf = df
  tempDf$index = c(1:nrow(df))
  goal_events = tempDf[tempDf$EventType == "GOAL",c("GameId","startInterval","endInterval","index")]
  
  
  game_ids = unique(goal_events$GameId)
  
  count = rep(0,nrow(df))
  
  for (id in game_ids) 
  {
    print(id)
    for (i in 1:length(start)) {
      
      s_ = start[i]
      e_ = end[i+1]
      
      gameData = goal_events[(goal_events$GameId %in% id & goal_events$startInterval %in% s_ & goal_events$endInterval  %in%  e_),]
      index = unlist(ifelse(nrow(gameData) > 0, gameData[1,"index"],0))
      if(index > 0)
      {
        count[index] = 1
      }
      
    }
  }
  
  tempDf$count1 = count
  return(tempDf)
}

############### count = 1 for end of interval
method2 = function(df){
  
  start = unique(df$startInterval)
  end =  unique(df$endInterval)
  
  tempDf = df
  tempDf$index = c(1:nrow(df))
  goal_events = tempDf[tempDf$EventType == "GOAL",c("GameId","startInterval","endInterval","index")]
  
  
  game_ids = unique(goal_events$GameId)
  
  count = rep(0,nrow(df))
  
  for (id in game_ids) 
  {
    print(id)
    for (i in 1:length(start)) {
      
      s_ = start[i]
      e_ = end[i+1]
      
      gameData = goal_events[(goal_events$GameId %in% id & goal_events$startInterval %in% s_ & goal_events$endInterval  %in%  e_),]
      index = unlist(ifelse(nrow(gameData) > 0, gameData[nrow(gameData),"index"],0))
      if(index > 0)
      {
        count[index] = 1
      }
      
    }
  }
  
  
  tempDf = df
  tempDf$count2 = count
  return(tempDf)
  
}


######Function calling ################
r = create_interval(120,df)
df$interval = r$interval


r = splitInterval(df)
##  spilt Intrerval start
df$startInterval = ifelse(is.na(r$startTime),0,r$startTime)
##  spilt Intrerval end
df$endInterval = ifelse(is.na(r$endTime),0,r$endTime)



r = method1(df)
df$count1 = r$count1


r = method2(df)
df$count2 = r$count2


###################

### Consider onlt three periods pf game for research
df <- df[which(data$PeriodNumber %in% c(1,2,3)), ]


save(df, file = "data/manpower.Rdata")











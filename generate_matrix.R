rm(list = ls())
#every data file is si filter for season 2013-2014
library(dplyr)
#files read
library(lubridate)

total_player = 9

event_goal = read.csv("data/event_goal.csv")
# game data for season 2013-2014
game = read.csv("data/game.csv")
# play by play events dataFrame
play_by_play_events = read.csv("data/pp_events.csv")


#filter out un used columns
goal_events = event_goal[,c("GameId", "AwayTeamId", 
                            "HomeTeamId", "PeriodNumber", 
                            "EventTime", "ScoringTeamId","GoalId")]


goal_events = game %>% left_join(goal_events)


#goal_events = goal_events[result,]


## calculate Manpower home and away?
ppp  =  play_by_play_events

## take gaol event into account 
ppp <- ppp[which(ppp$EventType =="GOAL"), ]

# manpower away 
manpower <- unlist(lapply(1:nrow(ppp), function(x) {
  total_player -  length(which(is.na(ppp[x,11:19])))
}))


ppp$manPowerAway = manpower


## manpowerHome
manpower <- unlist(lapply(1:nrow(ppp), function(x) {
  total_player -  length(which(is.na(ppp[x,20:28])))
}))

ppp$manPowerHome = manpower



## convert Eventtime into seconds
ppp$seconds<- as.numeric(hms(ppp$EventTime))
### manPowerDifference
ppp$manPowerDiff = ppp$manPowerHome - ppp$manPowerAway


resultMat  = ppp[,c("GameId","manPowerAway","manPowerHome","manPowerDiff")]

## calculate goal for and goal against


myMat = left_join(goal_events,ppp,by=c("GoalId"="ExternalEventId"))
#myMat = left_join(ppp,goal_events)
myMat = myMat[,c("GameId.x","Season.x","SeasonType.x","AwayTeamId.x","HomeTeamId.x","PeriodNumber.x","EventTime.x",
                 "EventType","GoalId","manPowerAway","manPowerHome","ScoringTeamId","seconds")]


#winer <- goal_events %>% 
#  group_by(GameId, ScoringTeamId) %>%
#  summarise(goals = n())

## rename Columns Names
myMat= myMat %>% 
  rename(
    GameId = GameId.x,
    Season = Season.x,
    SeasonType = SeasonType.x,
    HomeTeamId = HomeTeamId.x,
    AwayTeamId = AwayTeamId.x,
    PeriodNumber = PeriodNumber.x,
    EventTime = EventTime.x
  )



# get away team goals
myMat$AwayTeamGoals <- ifelse(myMat$ScoringTeamId == myMat$AwayTeamId, 1, 0)
# get home team goals
myMat$HomeTeamGoals <- ifelse(myMat$ScoringTeamId == myMat$HomeTeamId, 1, 0)



library(data.table)

df <- data.table(myMat)
GF <- df[, list(GF = cumsum(HomeTeamGoals)), by=list(GameId)]
GA <- df[, list(GA = cumsum(AwayTeamGoals)), by=list(GameId)]
df <- cbind(df, GF[, -1])
df <- cbind(df, GA[, -1])



t = df[,c("GameId","EventTime","seconds","PeriodNumber","GA","GF","HomeTeamGoals","AwayTeamGoals","ScoringTeamId","AwayTeamId","HomeTeamId","manPowerAway","manPowerHome","Season","SeasonType")]      

write.csv(t,"/Users/rabnawazjansher/Documents/732A76-Researc-Project/research_project_code/data/mymat.csv", row.names = FALSE)


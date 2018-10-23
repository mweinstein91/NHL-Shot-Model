library(tidyverse)
library(magrittr)
library(lme4)
library(broom)
library(lubridate)

df<- read_csv("nhl_pbp20172018.csv")
df  %<>% select(-X1)
#set interval of dates to get only regular season games
reg_season<-interval(ymd("2017-10-04"), ymd("2018-04-07"))

#filter all events into only events that contain a shot during regulation and 5x5. Create variables that determine the home team, goalie, and defending team.
shots <- df %>% filter(Event %in% c("SHOT", "GOAL", "MISS") & Period < 4 & Strength == "5x5" & Date %within% reg_season) %>% mutate(goalie = case_when(Ev_Team == Home_Team ~ Away_Goalie, Ev_Team == Away_Team ~ Home_Goalie),
                                                                                     home_field = case_when(Ev_Team == Home_Team ~ 1, Ev_Team == Away_Team ~ 0),
                                                                                  
                                                                                     defending_team = case_when(Ev_Team == Home_Team ~ Away_Team, Ev_Team == Away_Team ~ Home_Team))
                                                                            
#use regex to extract distances from each description for model.                                                                                     
shots %<>% mutate(distance = str_extract(Description, "\\d+(?= ft\\.)"))

#dummy encode the response variable, and tidy up the data for modeling
shots %<>% mutate(goal = case_when(Event == 'GOAL' ~ 1, Event != 'GOAL' ~ 0)) %>% select(goal, p1_name, p1_ID, Event, Description, Type, Period, home_field, distance, goalie, defending_team, Ev_Team) %>% rename(shooter = p1_name, attacking_team = Ev_Team) 

#turn distance into a numeric
shots$distance<-as.numeric(shots$distance)

#filter out the small number of shots with a null value
shots %<>% na.omit()

#fit model 
fit<-glmer(data = shots, goal ~  Type + distance + Type:distance + Period + home_field  + (1|shooter) + (1|goalie) + (1|defending_team) + (1|attacking_team), nAGQ = 0, family=binomial(link = "logit"))



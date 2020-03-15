# 3/12/2020
# adding shiny app
# (1) could alternatively build a shiny app for this with a dropdown of all the teams
#
# (2) it would be helpful to have a master table that renames schools from different data sources
#     it only makes sense to build that once the 64 schools are picked

# https://www.sports-reference.com/cbb/seasons/2020-school-stats.html
# https://en.wikipedia.org/wiki/2019_NCAA_Division_I_Men%27s_Basketball_Tournament

library(tidyverse)
library(readxl)
library(shiny)

team1 <- 'duke'
team2 <- 'clemson'

# import <- read_excel('/Users/horvasab/Desktop/bracket/sports_reference.xlsx', sheet = '2019') %>%
import1 <- read_excel('sports_reference.xlsx', sheet = '2019') %>%
  mutate(conference = paste(W_conf, "-", L_conf)) %>%
  mutate(record = paste(Wins, "-", Losses)) %>%
  mutate(home = paste(W_home, "-", L_home)) %>%
  mutate(away = paste(W_away, "-", L_away)) %>%
  mutate(school = tolower(School_trim)) %>%
  mutate(`3pg` = `3P`/Games) %>%
  mutate(FTpg = round(FT/Games)) %>%
  mutate(OREBpg = round(ORB/Games)) %>%
  mutate(REBpg = round(TRB/Games)) %>%
  mutate(ASTpg = round(AST/Games)) %>%
  mutate(STLpg = round(STL/Games)) %>%
  mutate(BLKpg = round(BLK/Games)) 
stats_2019 <- import1 %>%
  select(school, record, SimpleRatingSystem, StrengthOfSchedule, 
         home, away, conference, 'FG%', `3pg`, FTpg, 'FT%', OREBpg, REBpg,
         ASTpg, STLpg, BLKpg) %>%
  arrange(desc(SimpleRatingSystem)) 

import2 <- read_excel('ncaa basketball 2019-20.xlsx', sheet = 'Sheet1') %>%
  mutate(school = tolower(Team)) 
games <- import2 %>%
  select(school, Final, Rot, Date) %>%
  mutate(opponent_rot = ifelse(Rot %% 2 == 0, Rot - 1, Rot + 1) ) %>%
  mutate(game_date = paste(ifelse(sapply(Date,nchar)==3,'2020','2019'),Date, sep = '-'))
lookup <- import2 %>%
  mutate(opponent_rot = Rot) %>% 
  mutate(opponent_school = school) %>%
  mutate(opponent_final = Final) %>%
  select(opponent_rot, opponent_school, Date, opponent_final) %>%
  mutate(game_date = paste(ifelse(sapply(Date,nchar)==3,'2020','2019'),Date, sep = '-'))
games_2020 <- merge(games, lookup) %>%
  select(game_date, school, Final, opponent_school, opponent_final)

# matchup
compare <- function(team1, team2){
  team1_stats <- stats_2019 %>%
    filter(school == team1)
  team2_stats <- stats_2019 %>%
    filter(school == team2)
  x <- rbind(team1_stats, team2_stats)
  x
}

# games
matchup <- function(team1, team2){
  matchups <- games_2020 %>%
    filter(school == team1, opponent_school == team2) %>%
    select(game_date, school, Final, opponent_final, opponent_school)
  matchups
}

# last ten matches
games_last_ten <- function(team) {
  x <- games_2020 %>%
    filter(game_date > '2019-1231') %>%
    filter(school == team) %>%
    arrange(desc(game_date))
  top_n(x, 10)
}

# could alternatively build a shiny app for this with a dropdown of all the teams
compare <- compare(team1, team2)
compare
matchup <- matchup(team1, team2)
matchup
games_last_ten_team1 <- games_last_ten(team1)
games_last_ten_team1
games_last_ten_team2 <- games_last_ten(team2)
games_last_ten_team2

rm(games, games_2020, import1, import2, lookup, stats_2019)

# # further analysis 
# 
# # round 1 #1
# compare <- matchup('gonzaga', 'fairleigh dickinson') # gonzaga
# compare <- matchup('gonzaga', 'baylor') #gonzaga
# compare <- matchup('gonzaga', 'florida state') #toss up (gonzaga still won)
# compare <- matchup('gonzaga', 'texas tech') #toss up (texas tech won)
# # texas tech was an at-large bid but won 9 of last 10 games
# # texas tech lost to Virginia in the finals
# 
# # round 1 #1
# compare <- matchup('duke', 'north dakota state') # duke
# compare <- matchup('duke', 'central florida') # duke
# compare <- matchup('duke', 'virginia tech') # duke
# compare <- matchup('duke', 'michigan state') # toss up (michigan won)
# 
# # round 1 #1
# compare <- matchup('virginia', 'purdue') # toss up (virginia won)
# compare <- matchup('north carolina', 'washington') # maybe nc (washington won)
# 
# # last 10 games data
# # where was the game played (how far from home)
# # did the two teams already play each other?  who won?
# # instead, just do a count of the category wins, 
# #      me eyeballing these is noot a good indication
# #      and like i learned with conference winners, not always true
# # great players: http://www.espn.com/mens-college-basketball/statistics
# 
# 
# # do I want to weight certain stats
# # or should I just count the number of categories that each team is better by?
# 
# # ODD 	played against	The EVEN which is one higher than the odd
# # https://www.sportsbookreviewsonline.com/scoresoddsarchives/ncaabasketball/ncaabasketballoddsarchives.htm
# # this can look up if the teams played each other earlier in the season
# # this can also be used to calculate the last 10 games


# Created:      3/15/2020, Sabi Horvat
# Instructions: Run all of the code first, then run the app
#
#
# Once the schools are picked, will build a master table to align the school names
#     since the school names are slightly different in the two data sources
#
# data sources
# https://www.sports-reference.com/cbb/seasons/2020-school-stats.html
# https://en.wikipedia.org/wiki/2019_NCAA_Division_I_Men%27s_Basketball_Tournament

library(tidyverse)
library(readxl)
library(shiny)

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
  arrange(school)

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
  select(game_date, school, Final, opponent_school, opponent_final) %>%
  mutate(win_or_loss = ifelse(Final > opponent_final, 'W', 'L'))

rm(import1, import2, games, lookup)

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
    select(game_date, school, Final, opponent_final, opponent_school, win_or_loss)
  matchups
}

# last ten matches
games_last_ten <- function(team) {
  x <- games_2020 %>%
    filter(game_date > '2019-1231') %>%
    filter(school == team) %>%
    arrange(desc(game_date)) %>%
    top_n(10,game_date)   # slice(1:10) also works
}


# shiny app


shinyApp(
  ui = fluidPage(
    titlePanel("March Madness Shiny!"),
    selectInput("team1", 
                "Choose Team1",
                stats_2019$school
    ),
    selectInput("team2",
                "Choose Team2",
                stats_2019$school
    ),
    titlePanel("Statistic Comparison"),
    tableOutput("data1"),
    titlePanel("Head to Head Games"),
    tableOutput("data2"),
    titlePanel("Team1 last 10 games"),
    tableOutput("data3"),
    titlePanel("Team2 last 10 games"),
    tableOutput("data4")
    
  ),
  
  server = function(input, output) {
    output$data1 <- renderTable({
      compare(input$team1, input$team2)
    }, rownames = TRUE)    
    
    output$data2 <- renderTable({
      matchup(input$team1, input$team2)
    }, rownames = TRUE)    
    
    output$data3 <- renderTable({
      games_last_ten(input$team1)
    }, rownames = TRUE)    
    
    output$data4 <- renderTable({
      games_last_ten(input$team2)
    }, rownames = TRUE)    
    
  }
)

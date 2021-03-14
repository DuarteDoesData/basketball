# Created:      3/15/2020, Sabi Horvat
# Updated:      2021 data added 3/14/2021
# Instructions: Run all of the code first, then run the app
#
# Note that a thorough data analysis has not been conducted on the two data sources
#     It is possible some school names may be slightly different and need "alignment"
#
# Note that this is hardcoded for the 2020-2021 men's ncaa basketball season
#
# data sources
# https://www.sports-reference.com/cbb/seasons/2021-school-stats.html
# https://www.sportsbookreviewsonline.com/scoresoddsarchives/ncaabasketball/ncaabasketballoddsarchives.htm

library(tidyverse)
library(readxl)
library(shiny)

import1 <- read_csv('2021_sports_reference.csv') %>%
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
stats <- import1 %>%
  select(school, record, SimpleRatingSystem, StrengthOfSchedule, 
         home, away, conference, 'FG%', `3pg`, FTpg, 'FT%', OREBpg, REBpg,
         ASTpg, STLpg, BLKpg) %>%
  arrange(school)

import2 <- read_excel('ncaa basketball 2020-21.xlsx', sheet = 'Sheet1') %>%
  mutate(school = tolower(Team)) 
import2$school <- gsub("washingtonu", "washington", import2$school)
import2$school <- gsub("norfolkst", "norfolkstate", import2$school)
import2$school <- gsub("appalachianst", "appalachianstate", import2$school)
import2$school <- gsub("calsantabarbara", "uc-santabarbara", import2$school)
import2$school <- gsub("calsantabarb", "uc-santabarbara", import2$school)
import2$school <- gsub("usc", "southerncalifornia", import2$school)
import2$school <- gsub("e.washington", "easternwashington", import2$school)
import2$school <- gsub("vacommonwealth", "virginiacommonwealth", import2$school)
import2$school <- gsub("loyolachicago", "loyola(il)", import2$school)
import2$school <- gsub("houstonu", "houston", import2$school)
import2$school <- gsub("mountstmarys", "mountst.mary's", import2$school)
import2$school <- gsub("lsu", "louisianastate", import2$school)
import2$school <- gsub("ncgreensboro", "northcarolina-greensboro", import2$school)
import2$school <- gsub("byu", "brighamyoung", import2$school)

lookup1 <- import2 %>%
  select(school, Final, Rot, Date) %>%
  mutate(opponent_rot = ifelse(Rot %% 2 == 0, Rot - 1, Rot + 1) ) %>%
  mutate(game_date = paste(ifelse(sapply(Date,nchar)==3,'2021','2020'),Date, sep = '-'))
lookup2 <- import2 %>%
  mutate(opponent_rot = Rot) %>% 
  mutate(opponent_school = school) %>%
  mutate(opponent_final = Final) %>%
  select(opponent_rot, opponent_school, Date, opponent_final) %>%
  mutate(game_date = paste(ifelse(sapply(Date,nchar)==3,'2021','2020'),Date, sep = '-'))
games <- merge(lookup1, lookup2) %>%
  select(game_date, school, Final, opponent_school, opponent_final) %>%
  mutate(win_or_loss = ifelse(Final > opponent_final, 'W', 'L'))
rm(import1, import2, lookup1, lookup2)


# matchup
compare <- function(team1, team2){
  team1_stats <- stats %>%
    filter(school == team1)
  team2_stats <- stats %>%
    filter(school == team2)
  x <- rbind(team1_stats, team2_stats)
  x
}

# games
matchup <- function(team1, team2){
  matchups <- games %>%
    filter(school == team1, opponent_school == team2) %>%
    select(game_date, school, Final, opponent_final, opponent_school, win_or_loss)
  matchups
}

# last ten matches
games_last_ten <- function(team) {
  x <- games %>%
    filter(game_date > '2020-1231') %>%
    filter(school == team) %>%
    arrange(desc(game_date)) %>%
    top_n(10,game_date)   # slice(1:10) also works
}

# shiny app
shinyApp(
  ui = fluidPage(
    titlePanel("March Madness Shiny!"),
    selectInput("team1", 
                "Choose Team1 (Scroll or Delete-and-Type)",
                stats$school
    ),
    selectInput("team2",
                "Choose Team2 (Scroll or Delete-and-Type)",
                stats$school
    ),
    titlePanel("Statistic Comparison"),
    tableOutput("data1"),
    titlePanel("Head to Head Games"),
    tableOutput("data2"),
    titlePanel("Team1 Last 10 Games"),
    tableOutput("data3"),
    titlePanel("Team2 Last 10 Games"),
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


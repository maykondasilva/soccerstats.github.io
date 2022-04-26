# Load packages

library(tidyverse)
library(DataExplorer)

# Load data

df <- read_csv(file = './data/soccer.csv') %>%
filter(!is.na(player))


# Exploratory data analysis


introduce(df)

plot_intro(df, 
           title ="Introductory plot", 
           ggtheme =theme_bw(), 
           theme_config=theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)))

# Plot missing observations by feature

plot_missing(df, 
             title ="Plot missing observations by feature", 
             ggtheme =theme_bw(), 
             theme_config=theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)))

# Data includes 2232 observations and 400 variables

# Q1 Who are the top 5 most valuable soccer players, what is their nationality, position, team (squad), and how many goals did they score in the 2018/2019 season?

# Check that variables are the correct type 

str(df$value)
str(df$squad)
str(df$nationality)
str(df$position)

# Modify variables that were wrong variable type


df$squad <- as.factor(df$squad)
df$nationality <- as.factor(df$nationality)
df$position <- as.factor(df$position)

# do this with tidyverse

df <- df %>% mutate(value_millions = (value/1000000))

# Ranking of players by value and number of goal

df <-df %>%
arrange(desc(value)) %>%
mutate(ranking_player_value = 1:length(player)) %>%
arrange(desc(goals)) %>%
mutate(ranking_player_goals = 1:length(player))

# ----  Player's value ----

# Average value of players in all dataset 

df %>%
summarise(average_value_millions = round((mean(value_millions,na.rm=TRUE)),2)) %>%
arrange(desc(average_value_millions))

# Value top 5 by position

df %>%
group_by(position) %>%
arrange(ranking_player_value) %>%
slice(1:5) %>%
mutate(average_value_top_5 = round((mean(value,na.rm=TRUE)),2)) %>%
arrange(desc(average_value_top_5)) %>%
select(c(player,squad,nationality,position,goals,assists,value,ranking_player_value)) %>%
rename("Player"=player,"Team"=squad,"Nationality"=nationality,"Goals"=goals,"Assists"=assists,"Value"=value,"Overall ranking value"=ranking_player_value) %>%
gt() %>%
fmt_number(
columns = where(is.numeric), 
decimals = 0 ,
suffixing = TRUE) %>%
cols_align(
align = "center",
columns = everything()) %>%
tab_header(title = md("**Top 5 most valuable players by position**")) %>%
tab_source_note("Data from season 18/19 extracted from Kaggle")


# Value top 5 by nationality

df %>%
  group_by(nationality) %>%
  arrange(ranking_player_value) %>%
  slice(1:5) %>%
  mutate(average_value_top_5 = round((mean(value,na.rm=TRUE)),2)) %>%
  arrange(desc(average_value_top_5)) %>%
  select(c(player,squad,nationality,position,goals,assists,value,ranking_player_value)) %>%
  rename("Player"=player,"Team"=squad,"Nationality"=nationality,"Goals"=goals,"Assists"=assists,"Value"=value,"Overall ranking value"=ranking_player_value) %>%
  gt() %>%
  fmt_number(
    columns = where(is.numeric), 
    decimals = 0 ,
    suffixing = TRUE) %>%
  cols_align(
    align = "center",
    columns = everything()) %>%
  tab_header(title = md("**Top 5 most valuable players by nationality**")) %>%
  tab_source_note("Data from season 18/19 extracted from Kaggle")


# Value top 5 by team

df %>%
  group_by(squad) %>%
  arrange(ranking_player_value) %>%
  slice(1:5) %>%
  mutate(average_value_top_5 = round((mean(value,na.rm=TRUE)),2)) %>%
  arrange(desc(average_value_top_5)) %>%
  select(c(player,squad,nationality,position,goals,assists,value,ranking_player_value)) %>%
  rename("Player"=player,"Team"=squad,"Nationality"=nationality,"Goals"=goals,"Assists"=assists,"Value"=value,"Overall ranking value"=ranking_player_value) %>%
  gt() %>%
  fmt_number(
    columns = where(is.numeric), 
    decimals = 0 ,
    suffixing = TRUE) %>%
  cols_align(
    align = "center",
    columns = everything()) %>%
  tab_header(title = md("**Top 5 most valuable players by team**")) %>%
  tab_source_note("Data from season 18/19 extracted from Kaggle")
 
# ----  Player's goals ----

# Average goals of players in all dataset 

df %>%
  summarise(goals = round((mean(goals,na.rm=TRUE)),2)) %>%
  arrange(desc(goals))

# Average goals of players by position

df %>%
  group_by(position) %>%
  arrange(ranking_player_goals) %>%
  slice(1:5) %>%
  mutate(average_goals_top_5 = round((mean(goals,na.rm=TRUE)),2)) %>%
  arrange(desc(average_goals_top_5)) %>%
  select(c(player,squad,nationality,position,goals,assists,value,ranking_player_goals)) %>%
  rename("Player"=player,"Team"=squad,"Nationality"=nationality,"Goals"=goals,"Assists"=assists,"Value"=value,"Overall ranking goals"=ranking_player_goals) %>%
  gt() %>%
  fmt_number(
    columns = where(is.numeric), 
    decimals = 0 ,
    suffixing = TRUE) %>%
  cols_align(
    align = "center",
    columns = everything()) %>%
  tab_header(title = md("**Top 5 highest goal scorers by position**")) %>%
  tab_source_note("Data from season 18/19 extracted from Kaggle")



# Average goals of players by nationality

df %>%
  group_by(nationality) %>%
  arrange(ranking_player_goals) %>%
  slice(1:5) %>%
  mutate(average_goals_top_5 = round((mean(goals,na.rm=TRUE)),2)) %>%
  arrange(desc(average_goals_top_5)) %>%
  select(c(player,squad,nationality,position,goals,assists,value,ranking_player_goals)) %>%
  rename("Player"=player,"Team"=squad,"Nationality"=nationality,"Goals"=goals,"Assists"=assists,"Value"=value,"Overall ranking goals"=ranking_player_goals) %>%
  gt() %>%
  fmt_number(
    columns = where(is.numeric), 
    decimals = 0 ,
    suffixing = TRUE) %>%
  cols_align(
    align = "center",
    columns = everything()) %>%
  tab_header(title = md("**Top 5 highest goal scorers by nationality**")) %>%
  tab_source_note("Data from season 18/19 extracted from Kaggle")


# Average goals of players by team

df %>%
  group_by(squad) %>%
  arrange(ranking_player_goals) %>%
  slice(1:5) %>%
  mutate(average_goals_top_5 = round((mean(goals,na.rm=TRUE)),2)) %>%
  arrange(desc(average_goals_top_5)) %>%
  select(c(player,squad,nationality,position,goals,assists,value,ranking_player_goals)) %>%
  rename("Player"=player,"Team"=squad,"Nationality"=nationality,"Goals"=goals,"Assists"=assists,"Value"=value,"Overall ranking goals"=ranking_player_goals) %>%
  gt() %>%
  fmt_number(
    columns = where(is.numeric), 
    decimals = 0 ,
    suffixing = TRUE) %>%
  cols_align(
    align = "center",
    columns = everything()) %>%
  tab_header(title = md("**Top 5 highest goal scorers by team**")) %>%
  tab_source_note("Data from season 18/19 extracted from Kaggle")


# Q2 What are the soccer teams (squad) and leagues for which the top scorers played?

# Q3 What are the top 10 nationalities with the highest number of goals scored by their top 5 scorers?
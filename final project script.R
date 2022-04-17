# Load packages

library(tidyverse)
library(DataExplorer)

# Load data

df <- read.csv(file = './data/transfermarkt_fbref_201819.csv', header = TRUE)

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

# 1 Who are the top 5 most valuable soccer players, what is their nationality, position, team (squad), and how many goals did they score in the 2018/2019 season?

df %>%
summarise(average_value_millions = round((mean(value,na.rm=TRUE)/1000000),2) ,average_goals= round(mean(goals,na.rm=TRUE),2))


df.2 <-df %>%
filter(!is.na(value),!is.na(goals)) %>%
arrange(desc(value)) %>%
mutate(ranking_player_value = 1:length(player)) %>%
arrange(goals) %>%
mutate(ranking_goals = 1:length(player))


# 3.2 What are the soccer teams (squad) and leagues for which the top scorers played?

# 3.3 What are the top 10 nationalities with the highest number of goals scored by their top 5 scorers?
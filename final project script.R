# Load packages

library(tidyverse)
library(DataExplorer)

# Load data

df <- read_csv(file = './data/soccer.csv')

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

df %>%
summarise(average_value_millions = round((mean(value,na.rm=TRUE)/1000000),2) ,average_goals= round(mean(goals,na.rm=TRUE),2))

# do this with tidyverse

df$value_millions <- (df$value)/1000000

# Visualizations with ggplot

hist(df$value_millions)

df <-df %>%
arrange(desc(value)) %>%
mutate(ranking_player_value = 1:length(player)) %>%
arrange(desc(goals)) %>%
mutate(ranking_goals = 1:length(player))

# Density plot of number of goals by player -- divide by nationality



# Q2 What are the soccer teams (squad) and leagues for which the top scorers played?

# Q3 What are the top 10 nationalities with the highest number of goals scored by their top 5 scorers?
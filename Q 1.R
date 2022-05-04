# Load packages

library(tidyverse)
library(DataExplorer)
library(forcats)
library(gt)
library(janitor)
library(paletteer)
library(webshot)
library(magrittr)
library(purrr)
library(cfbplotR)
library(sf)
library(leaflet)
library(plotly)
library(readr)

# Set plots aspect

theme_set(theme_bw() + theme(plot.title = element_text(hjust = 0.5)))

# ---- Data cleaning (I did modify how dataset was uploaded to R) -----

# We need to upload all the data in the first part of the script

# Load data
soccer.df <- read_csv(file = './data/soccer.csv') %>%
  filter(!is.na(player))

ioc_country.df <- read_csv(url("https://raw.githubusercontent.com/datasets/country-codes/master/data/country-codes.csv")) %>%
  select(FIFA,official_name_en) %>%
  rename(Country=official_name_en,NOC=FIFA)


world_boundaries3.df <- read_csv("./data/world-coordinates-data.csv") %>%
janitor::clean_names() 


# Create variable for millions of dollars
soccer.df <- soccer.df %>%
  mutate(value_millions = (value/1000000)) %>%
  separate(nationality, into = c("nationality", "country_origin"), sep = " ")

# Ranking of players by value and number of goal
soccer.df <- soccer.df %>%
  arrange(desc(value)) %>%
  mutate(ranking_player_value = 1:length(player)) %>%
  arrange(desc(goals)) %>%
  mutate(ranking_player_goals = 1:length(player))

# Create variable for lat and long

world_boundaries3.df <- world_boundaries3.df %>%
  separate(geo_point_coord, into = c("lat", "long"), sep = ",") %>%
  mutate(lat = as.numeric(lat),long = as.numeric(long))


# Join country's names

soccer_world.df <- left_join(
  soccer.df,
  ioc_country.df,
  by = c("country_origin" = "NOC") #zipcode in first, zip_code second
)


# Edit names for countries from United Kingdom, and other countries with special characters

soccer_world.df <- soccer_world.df %>%
  mutate(Country = ifelse(country_origin %in% "ENG"|country_origin %in% "WAL"|country_origin %in% "SCO"|country_origin %in% "NIR","United Kingdom",Country)) %>%
  mutate(Country = ifelse(country_origin %in% "CUW","Curaçao",Country)) %>%
  mutate(Country = ifelse(country_origin %in% "KVX","Kosovo",Country)) %>%
  mutate(Country = ifelse(country_origin %in% "CIV","Ivory Coast",Country)) %>%
  mutate(Country = ifelse(country_origin %in% "CUW","Curacao",Country)) %>%
  mutate(Country = ifelse(nationality %in% "GPE","Curacao",Country)) %>%
  mutate(Country = ifelse(country_origin %in% "REU","Reunion",Country))


# Join country's shape files

soccer_world3.df <- inner_join(
  soccer_world.df, 
  world_boundaries3.df, 
  by = c("Country" = "english_name") 
)


# ---- Q1 -----
# Table with most valuable players by position (Position were grouped by Foward, Midfielder, Defender and Goalkeeper)
soccer_world.df %>%
  mutate(Position = ifelse(grepl("Forward", position2),"Forward",position2)) %>%
  mutate(Position = ifelse(grepl("Midfielder", Position),"Midfielder",Position)) %>%
  mutate(Position = ifelse(grepl("Defender", Position),"Defender",Position)) %>%
  group_by(Position) %>%
  arrange(ranking_player_value) %>%
  slice(1:5) %>%
  mutate(average_value_top_5 = round((mean(value,na.rm = TRUE)),2)) %>%
  arrange(desc(average_value_top_5)) %>%
  select(c(player,squad,Country,Position,goals,assists,value)) %>%
  rename("Player" = player,"Team" = squad,"Goals" = goals,"Assists" = assists,"Value (USD)" = value) %>%
  gt() %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 0 ,
    suffixing = TRUE) %>%
  cols_align(
    align = "center",
    columns = everything()) %>%
  cols_align(
    align = "center",
    columns = everything()) %>%
  data_color(
    columns =Goals,
    colors = scales::col_numeric(
      palette = c("PuBu"),
      domain  = c(min(Goals), max(Goals)))) %>%
  data_color(
    columns =Assists,
    colors = scales::col_numeric(
      palette = c("PuBu"),
      domain  = c(min(Assists), max(Assists)))) %>%
  data_color(
    columns =`Value (USD)`,
    colors = scales::col_numeric(
      palette = c("Purples"),
      domain  = c(min(`Value (USD)`), max(`Value (USD)`)))) %>%
  summary_rows(
    groups  = TRUE,
    columns = where(is.numeric),
    fns = list("# Average top 5" = ~mean(., na.rm = TRUE)),
    formatter = fmt_number,
    decimals  = 0,
    suffixing = TRUE) %>%
  tab_header(title = md("**Top 5 most valuable players by position**")) %>%
  tab_source_note("*Data merged from transfermarkt.de and fbref.com by Rafael Stepien and available on Kaggle*")

# Plot most valuable players by team
Plot_valuable_players <- soccer_world.df %>%
  group_by(league) %>%
  arrange(ranking_player_value) %>%
  slice(1:5) %>%
  mutate(average_value_top_5 = round((mean(value,na.rm = TRUE)),2)) %>%
  arrange(desc(average_value_top_5)) %>%
  mutate(
    text_label = str_c("Value, Nationality, and Squad and More",
                       "\nTeam: ", squad,
                       "\nValue: ", value_millions,
                       "\nGoals: ", goals,
                       "\nAssists: ", assists,
                       "\nPosition: ", position2)) %>%
  rename("Player" = player,"Team" = squad,"Value" = value_millions,"Goals"=goals) %>%
  ggplot() +
  geom_col(aes(x = Value, y = fct_reorder(Player,value), fill=Goals, text = text_label)) +
  labs(
    x     = "Value (millions USD)",
    y     = "Player",
    title = "Most valuable players in top 5 European Leagues" ) +
  scale_fill_viridis_c() +
  theme(axis.text.x = element_text(angle   = 90,
                                   vjust = 0.5,
                                   hjust = 1,
                                   size  = 10)) +
  facet_wrap( ~league , scales = "free",ncol=2) +
  theme(panel.spacing = unit(2, "lines"))

# Transform into a ggplotly object
ggplotly(Plot_valuable_players, tooltip = "text")
  
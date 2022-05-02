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
library(lubridate)
library(ggridges)
library(tigris)
library(ggsflabel)
library(ggthemes)
library(htmltools)
library(htmlwidgets)

# Load data

soccer.df <- read_csv(file = './data/soccer.csv') %>%
filter(!is.na(player))


# Exploratory data analysis


introduce(soccer.df)

plot_intro(soccer.df, 
           title ="Introductory plot", 
           ggtheme =theme_bw(), 
           theme_config=theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)))

# Plot missing observations by feature

plot_missing(soccer.df, 
             title ="Plot missing observations by feature", 
             ggtheme =theme_bw(), 
             theme_config=theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)))

# Data includes 2232 observations and 400 variables

# Q1 Who are the top 5 most valuable soccer players, what is their nationality, position, team (squad), and how many goals did they score in the 2018/2019 season?

# Check that variables are the correct type 

str(soccer.df$value)
str(soccer.df$squad)
str(soccer.df$nationality)
str(soccer.df$position)

# Modify variables that were wrong variable type


soccer.df$squad <- as.factor(soccer.df$squad)
soccer.df$nationality <- as.factor(soccer.df$nationality)
soccer.df$position <- as.factor(soccer.df$position)

# do this with tidyverse

soccer.df <- soccer.df %>% 
  mutate(value_millions = (value/1000000)) %>%
  separate(nationality, into = c("nationality", "country_origin"), sep = " ")

# Ranking of players by value and number of goal

soccer.df <- soccer.df %>%
arrange(desc(value)) %>%
mutate(ranking_player_value = 1:length(player)) %>%
arrange(desc(goals)) %>%
mutate(ranking_player_goals = 1:length(player))

# ----  Player's value ----

# Average value of players in all dataset 

soccer.df %>%
summarise(average_value_millions = round((mean(value_millions,na.rm=TRUE)),2)) %>%
arrange(desc(average_value_millions))

# Value top 5 by position

soccer.df %>%
group_by(position) %>%
arrange(ranking_player_value) %>%
slice(1:5) %>%
mutate(average_value_top_5 = round((mean(value,na.rm = TRUE)),2)) %>%
arrange(desc(average_value_top_5)) %>%
select(c(player,squad,nationality,position,goals,assists,value,ranking_player_value)) %>%
rename("Player" = player,"Team" = squad,"Nationality" = nationality,"Goals" = goals,"Assists" = assists,"Value" = value,"Overall ranking value" = ranking_player_value) %>%
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

soccer.df %>%
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

soccer.df %>%
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

soccer.df %>%
  summarise(goals = round((mean(goals,na.rm=TRUE)),2)) %>%
  arrange(desc(goals))

# Average goals of players by position

soccer.df %>%
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

soccer.df %>%
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

soccer.df %>%
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
soccer.df %>%
  select(player, squad, league, goals) %>%
  arrange(desc(goals)) %>%
  slice(1:20) %>%
  rename(
    "Player" = player, 
    "Team"   = squad, 
    "League" = league, 
    "Goals"  = goals) %>%
  gt() %>%
  tab_header("Top 20 Scorers - Teams and Leagues") %>%
  data_color(columns = "Goals",
             colors = scales::col_numeric(
               palette = c("Purples"),
               domain  = c(10, 40))) %>%
  data_color(columns = "League",
             colors = scales::col_factor(
               palette = c("Spectral"),
               domain  = c("La Liga", "Ligue 1", "Serie A", "Bundesliga", "Premier League")))

s1 <- soccer.df %>%
  select(player, squad, league, country_origin, goals, assists, value_millions, position2) %>%
  arrange(desc(goals)) %>%
  slice(1:20) %>%
  mutate(player = fct_reorder(player, desc(goals))) %>%
  mutate(
    text_label = str_c("\nNationality: ", country_origin,
                       "\nTeam: ", squad,
                       "\nValue (M Euros): ", value_millions,
                       "\nAssists: ", assists,
                       "\nPosition: ", position2)) %>%
  rename("League" = league) %>%
  ggplot() +
    geom_col(aes(x = player, y = goals, fill = League, text = text_label)) +
  labs(
    x     = "Player",
    y     = "Goals",
    title = "Number of Goals by Player"
  ) +
  scale_fill_manual(values = c("#B2182B", "#D6604D", "#F4A582", "#4393C3", "#2166AC")) +
  theme(axis.text.x = element_text(angle   = 0,
                                   vjust = 0.5,
                                   hjust = 1,
                                   size  = 10)) +
  coord_flip()

ggplotly(s1, tooltip = "text")

## Now using Plotly
soccer.df %>%
    select(player, country_origin, squad, league, goals, assists, value_millions, position2) %>%
    arrange(desc(goals)) %>%
    slice(1:20) %>%
  mutate(player = fct_reorder(player, desc(goals))) %>%
  plot_ly(
    x = ~player,
    y = ~goals,
    type = "bar",
    mode = "markers",
    hoverinfo = "text",
    text = ~paste("</br> Team:", squad,
                  "</br> Value (M Euros):", value_millions,
                  "</br> Position:", position2,
                  "</br> Assistis:", assists),
    color = ~country_origin,
    colors = "viridis"
  ) %>%
  layout(
    title  = "Number of Goals by Player",
    xaxis  = list(title = "Player"),
    yaxis  = list(title = "Goals"))
  

# Q3 What are the top 10 nationalities with the highest number of goals scored by their top 5 scorers?

# Top 5 scorers by country
top5_players <- soccer.df %>%
  group_by(country_origin, player) %>%
  summarise(total_goals = sum(goals)) %>%
  arrange(country_origin, desc(total_goals)) %>%
  slice(1:5) %>%
  ungroup()

# Top 10 coountries by number of goals scored by top 5 scorers
top10_countries <- soccer.df %>%
  filter(player %in% top5_players$player) %>%
  group_by(country_origin) %>%
  summarise(total_goals_country = sum(goals)) %>%
  arrange(desc(total_goals_country)) %>%
  slice(1:10)

# GT table
goals_10countries <- soccer.df %>%
  filter(country_origin %in% top10_countries$country_origin,
         player %in% top5_players$player) %>%
  group_by(country_origin) %>%
  mutate(total_goals_country = sum(goals)) %>%
  select(player, country_origin, squad, league, goals, total_goals_country) %>%
  arrange(desc(total_goals_country)) %>%
  select(player, country_origin, squad, league, goals) %>%
  rename("Player" = player, "Team" = squad, "League" = league, "Goals" = goals) %>%
  gt() %>%
  summary_rows(
    groups  = TRUE,
    columns = contains(c("Goals")),
    fns = list("# Goals per Country" = ~sum(., na.rm = TRUE)),
    formatter = fmt_number,
    decimals  = 0,
    suffixing = TRUE
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>% 
  cols_align(align = c("center"), columns = everything()) %>%
  opt_table_lines(extent = "default") %>%
  tab_options(
    column_labels.border.top.color = "black",
    column_labels.border.top.width = px(2),
    column_labels.border.bottom.color = "black",
    table_body.hlines.color   = "white",
    table.border.bottom.color = "black",
    table.border.bottom.width = px(2)
  ) %>%
  data_color(
    columns = "Goals",
    colors = scales::col_numeric(
      palette = c("PuBu"),
      domain  = c(0, 40))) %>%
  tab_source_note(
    source_note = md("*Data merged from transfermarkt.de and fbref.com by Rafael Stepien and available on Kaggle*"))
  
## Q3 - FIGURE = GRAPH

goals_10countries_ggplot <- soccer.df %>%
  filter(country_origin %in% top10_countries$country_origin,
         player %in% top5_players$player) %>%
  group_by(country_origin) %>%
  summarise(total_goals_country = sum(goals)) %>%
  ungroup() %>%
  mutate(country_origin = fct_reorder(country_origin, total_goals_country, .desc = TRUE)) %>%
  ggplot() +
  geom_col(aes(x = country_origin, y = total_goals_country, fill = country_origin)) +
  scale_fill_viridis_d("Country") +
  labs(title = "Top 10 Countries by No. of Goals from their Top 5 Scorers",
       x = "Country of Origin",
       y = "Number of Goals") +
  theme(legend.position = "none")


#### LEAFLET

ioc_country.df <- read_csv("./data/NOC_countries.csv") %>%
  select(Country, NOC)

soccer_world.df <- left_join(
  soccer.df, 
  ioc_country.df, 
  by = c("country_origin" = "NOC") #zipcode in first, zip_code second
) 

top5_players_new <- soccer_world.df %>%
  group_by(Country, player) %>%
  summarise(total_goals = sum(goals)) %>%
  arrange(Country, desc(total_goals)) %>%
  slice(1:5) %>%
  ungroup()

top10_countries_new <- soccer_world.df %>%
  filter(player %in% top5_players_new$player) %>%
  group_by(Country) %>%
  summarise(total_goals_country = sum(goals)) %>%
  arrange(desc(total_goals_country)) %>%
  slice(1:10)

goals_10countries_new <- soccer_world.df %>%
  filter(Country %in% top10_countries_new$Country,
         player %in% top5_players_new$player) %>%
  group_by(Country) %>%
  mutate(total_goals_country = sum(goals)) %>%
  select(player, Country, squad, league, goals, total_goals_country) %>%
  arrange(desc(total_goals_country)) %>%
  select(player, Country, squad, league, goals) %>%
  rename("Player" = player, "Team" = squad, "League" = league, "Goals" = goals) %>%
  gt() %>%
  summary_rows(
    groups  = TRUE,
    columns = contains(c("Goals")),
    fns = list("# Goals per Country" = ~sum(., na.rm = TRUE)),
    formatter = fmt_number,
    decimals  = 0,
    suffixing = TRUE
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>% 
  cols_align(align = c("center"), columns = everything()) %>%
  opt_table_lines(extent = "default") %>%
  tab_options(
    column_labels.border.top.color = "black",
    column_labels.border.top.width = px(2),
    column_labels.border.bottom.color = "black",
    table_body.hlines.color   = "white",
    table.border.bottom.color = "black",
    table.border.bottom.width = px(2)
  ) %>%
  data_color(
    columns = "Goals",
    colors = scales::col_numeric(
      palette = c("PuBu"),
      domain  = c(0, 40))) %>%
  tab_source_note(
    source_note = md("*Data merged from transfermarkt.de and fbref.com by Rafael Stepien and available on Kaggle*"))


### Joining soccer.df data with world boundaries data

world_boundaries.df <- read_csv("./data/world_coordinates.csv") %>%
  janitor::clean_names() %>%
  select(geo_point_coord, english_name, continent)

world_boundaries.df <- world_boundaries.df %>%
  separate(geo_point_coord, into = c("lat", "long"), sep = ",") %>%
  mutate(lat = as.numeric(lat),
         long = as.numeric(long))

soccer_world2.df <- inner_join(
  soccer_world.df, 
  world_boundaries.df, 
  by = c("Country" = "english_name") 
)

# Initialize the color pallete (continuous)
my_pallete <- colorFactor(c("BuPu"), 
                          domain = soccer_world2.df$Country)


# Number of players per country
soccer_world2.df <- soccer_world2.df %>%
  group_by(Country) %>%
  mutate(no_players = n(),
         no_goals = sum(goals))

#Add a text label like normal
soccer_world2.df <- soccer_world2.df %>%
  mutate(
    text_label = str_c(Country,
                       "<br/># Players: ",
                       no_players,
                       "<br/># Goals per Country: ",
                       no_goals
    )
  )

# Create a leaflet 
leaflet(data = soccer_world2.df) %>%
  addProviderTiles('CartoDB.Positron') %>% 
  addMarkers(lng = ~long, 
             lat = ~lat,
             label = ~map(text_label, HTML)
  )

# Color palette for continents
pal <- colorFactor(c("viridis"), domain = c("Europe", "Asia", "Africa", "Oceania", "Americas"))
leaflet_title <- c("Number of Soccer Players by Country")

leaflet(data = soccer_world2.df) %>%
  addProviderTiles('CartoDB.Positron') %>% 
  addCircleMarkers(lng = ~long, 
                   lat = ~lat,
                   radius = ~(no_players/20),
                   color = ~pal(continent),
                   opacity = 0.4,
                   label = ~map(text_label, HTML)
  ) %>%
  addLegend(
    title    = "Continent",
    pal      = pal,
    values   = ~soccer_world2.df$continent,
    position = "bottomright"
  ) %>%
  addControl(leaflet_title, position = "bottomleft")

  
## Trying other things

leaflet(data = soccer_world2.df) %>%
  addProviderTiles('CartoDB.Positron') %>% 
  addCircleMarkers(lng = ~long, 
             lat = ~lat,
             options = markerOptions(col = ~continent),
             color = ~pal,
             radius = ~(no_players),
             stroke = TRUE, fillOpacity = 1,
             label = ~map(text_label, HTML),
             clusterOptions = markerClusterOptions()
  )

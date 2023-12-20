
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibbletime)

rolling_mean <- rollify(mean, window = 10)



df <- read.csv('catan.csv') %>%
  filter(!is.na(game_id))

df_game <- df %>%
  select(game_id, date, num_players) %>%
  distinct() %>%
  mutate(date = as.Date(date, '%m/%d/%Y'))

df_player <- df %>%
  select(!c(date, num_players))

df_matt <- df_player %>%
  filter(name == 'Matt') %>%
  select(game_id, place:road_vp) %>%
  mutate(did_play = 1)
colnames(df_matt)[2:9] <- paste0(colnames(df_matt)[2:9], '_matt') 

df_bryan <- df_player %>%
  filter(name == 'Bryan') %>%
  select(game_id, place:road_vp) %>%
  mutate(did_play = 1)
colnames(df_bryan)[2:9] <- paste0(colnames(df_bryan)[2:9], '_bryan') 

df_zach <- df_player %>%
  filter(name == 'Zach') %>%
  select(game_id, place:road_vp) %>%
  mutate(did_play = 1)
colnames(df_zach)[2:9] <- paste0(colnames(df_zach)[2:9], '_zach') 

df_other <- df_player %>%
  filter(!(name %in% c('Matt', 'Bryan', 'Zach'))) %>%
  select(game_id) %>%
  distinct() %>%
  mutate(did_play_other = 1)

df_analysis <- df_game %>%
  left_join(df_matt, by = 'game_id') %>%
  left_join(df_bryan, by = 'game_id') %>%
  left_join(df_zach, by = 'game_id') %>%
  filter(did_play_matt == 1 & did_play_bryan == 1 & did_play_zach == 1 & num_players == 3) %>%
  mutate(did_win_matt = ifelse(place_matt == 1, 1, 0)) %>%
  mutate(did_win_bryan = ifelse(place_bryan == 1, 1, 0)) %>%
  mutate(did_win_zach = ifelse(place_zach == 1, 1, 0)) %>%
  mutate(games_played_cumulative = cumsum(did_play_matt)) %>%
  mutate(matt_win_pct_cumulative = cumsum(did_win_matt)/games_played_cumulative) %>%
  mutate(bryan_win_pct_cumulative = cumsum(did_win_bryan)/games_played_cumulative) %>%
  mutate(zach_win_pct_cumulative = cumsum(did_win_zach)/games_played_cumulative) %>%
  mutate(matt_win_cumulative = cumsum(did_win_matt)) %>%
  mutate(bryan_win_cumulative = cumsum(did_win_bryan)) %>%
  mutate(zach_win_cumulative = cumsum(did_win_zach)) %>%
  mutate(matt_win_pct_10gmrolling = rolling_mean(did_win_matt)) %>%
  mutate(bryan_win_pct_10gmrolling = rolling_mean(did_win_bryan)) %>%
  mutate(zach_win_pct_10gmrolling = rolling_mean(did_win_zach))


df_plot <- df %>%
  filter(name %in% c('Matt', 'Bryan', 'Zach', 'Andrew')) %>%
  group_by(name) %>%
  summarize(
    Settlement = mean(settlement_vp),
    City = mean(city_vp),
    Dev_Card = mean(dev_vp),
    Army = mean(army_vp),
    Road = mean(road_vp)
  ) %>%
  tibble::column_to_rownames('name') %>%
  mutate(Settlement = (Settlement - min(Settlement))/(max(Settlement) - min(Settlement))) %>%
  mutate(City = (City - min(City))/(max(City) - min(City))) %>%
  mutate(Dev_Card = (Dev_Card - min(Dev_Card))/(max(Dev_Card) - min(Dev_Card))) %>%
  mutate(Army = (Army - min(Army))/(max(Army) - min(Army))) %>%
  mutate(Road = (Road - min(Road))/(max(Road) - min(Road)))

max_min <- data.frame(
  Settlement = c(1, 0), 
  City = c(1, 0), 
  Dev_Card = c(1, 0),
  Army = c(1, 0), 
  Road = c(1, 0)
)
rownames(max_min) <- c("Max", "Min")

# Bind the variable ranges to the data
df_plot <- rbind(max_min, df_plot)

create_beautiful_radarchart <- function(data, 
                                        axistype = 0,
                                        color = "#00AFBB", 
                                        vlabels = colnames(data),
                                        vlcex = 0.7,
                                        caxislabels = NULL, 
                                        title = NULL, ...)
{
  radarchart(
    data, 
    axistype = axistype,
    # Customize the polygon
    pcol = color, 
    pfcol = scales::alpha(color, 0.2), 
    plwd = 2, 
    plty = 1,
    # Customize the grid
    cglcol = "grey", 
    cglty = 1, 
    cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, 
    vlabels = vlabels,
    caxislabels = caxislabels, 
    title = title, 
    ...
  )
}

# Solo graph
data <- df_plot[c("Max", "Min", "Bryan"), ]
op <- par(mar = c(1, 2, 2, 1))
create_beautiful_radarchart(student1_data, axistype = 0)
par(op)


# Same graph
op <- par(mar = c(1, 2, 2, 2))

create_beautiful_radarchart(
  data = df_plot,
  color = c("darkgreen", "#ce2029", "#fd5800", "#000080"),
  caxislabels = c()
)


# Add an horizontal legend
legend(
  x = "bottom", legend = rownames(df_plot[-c(1,2),]), horiz = TRUE,
  bty = "n", pch = 20 , col = c("darkgreen", "#ce2029", "#fd5800", "#000080"),
  text.col = "black", cex = 1, pt.cex = 1.5
)

# Factored graphs

# Define colors and titles
colors <- c("darkgreen", "#ce2029", "#fd5800", "#000080")
titles <- c("Andrew", "Bryan", "Matt", "Zach")

# Reduce plot margin using par()
# Split the screen in 3 parts
op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))

# Create the radar chart
for(i in 1:4){
  create_beautiful_radarchart(
    data = df_plot[c(1, 2, i+2), ],
    color = colors[i], 
    title = titles[i]
  )
}
par(op)



df_plot <- df %>%
  filter(name %in% c('Matt', 'Bryan', 'Zach', 'Andrew') & place == 1) %>%
  group_by(name) %>%
  summarize(
    Settlement = mean(settlement_vp),
    City = mean(city_vp),
    Dev_Card = mean(dev_vp),
    Army = mean(army_vp),
    Road = mean(road_vp)
  ) %>%
  tibble::column_to_rownames('name') %>%
  mutate(Settlement = (Settlement - min(Settlement))/(max(Settlement) - min(Settlement))) %>%
  mutate(City = (City - min(City))/(max(City) - min(City))) %>%
  mutate(Dev_Card = (Dev_Card - min(Dev_Card))/(max(Dev_Card) - min(Dev_Card))) %>%
  mutate(Army = (Army - min(Army))/(max(Army) - min(Army))) %>%
  mutate(Road = (Road - min(Road))/(max(Road) - min(Road)))

max_min <- data.frame(
  Settlement = c(1, 0), 
  City = c(1, 0), 
  Dev_Card = c(1, 0),
  Army = c(1, 0), 
  Road = c(1, 0)
)
rownames(max_min) <- c("Max", "Min")
df_plot <- rbind(max_min, df_plot)



# Define colors and titles
colors <- c("darkgreen", "#ce2029", "#fd5800", "#000080")
titles <- c("Andrew", "Bryan", "Matt", "Zach")

# Reduce plot margin using par()
# Split the screen in 3 parts
op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))

# Create the radar chart
for(i in 1:4){
  create_beautiful_radarchart(
    data = df_plot[c(1, 2, i+2), ],
    color = colors[i], 
    title = titles[i]
  )
}
par(op)

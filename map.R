library(dplyr)
library(stringr)
library(ggplot2)
library(usmap)
library(maps)

jail_data <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-jail-pop.csv?raw=true", stringsAsFactors = FALSE)

state_shape <- map_data("state")

# Map: The percentage of male in Jail in the United States for different counties

state_male_jail_2018 <- jail_data %>%
  filter(year == 2018) %>%
  select(state, male_jail_pop, total_jail_pop) %>%
  na.omit() %>%
  group_by(state) %>%
  summarise(percentage = mean(male_jail_pop/total_jail_pop, na.rm = TRUE))

state_df <- data.frame(state = state.abb, region = tolower(state.name))

state_male_jail_2018 <- state_male_jail_2018 %>%
  left_join(state_df)

ggplot() + 
  geom_map(data = state_shape, map = state_shape, aes(x = long, y = lat, map_id = region)) +
  geom_map(data = state_male_jail_2018, map = state_shape, aes(fill = percentage, map_id = region)) +
  coord_map("albers", lat0 = 40, lat1 = 45) +
  labs(title = "Percentage of Male Jail Population in United States", x = "Longitude", y = "Latitutde" ) +
  theme_minimal()
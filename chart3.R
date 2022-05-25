library("tidyverse")

prison_pop_data <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-jail-rates-1990.csv?raw=true")

# Chart 3: The change of jail rate for male overtime

male_rate <- prison_pop_data %>% 
  group_by(year) %>%
  summarise(male_total = sum(male_jail_pop_rate, na.rm = TRUE)) %>%
  filter(year >= 1990) %>%
  mutate(pct_change = ((male_total - lag(male_total, n = 1)) / male_total * 100))

ggplot(data = male_rate) +
  geom_col(aes(x = year, y = pct_change), fill = "red", color = "black") +
  labs(title = "The Rate Change of Male in Jail Overtime", x = "Time(year)", y = "% change in jail rate") +
  scale_x_continuous(breaks = round(seq(1990, 2018, by = 1))) +
  scale_y_continuous(breaks = seq(-15, 15, 2)) +
  theme(axis.text.x = element_text(angle = 90))
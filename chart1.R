library(tidyr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)

# Chart 1: Trends overtime for the jail rate for different genders

jail_pop_data <- read.csv('https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-jail-rates-1990.csv?raw=true', stringsAsFactors = FALSE)

female_sum <- jail_pop_data %>%
  group_by(year) %>%
  summarize(female = sum(female_jail_pop_rate, na.rm = TRUE))

male_sum <- jail_pop_data %>%
  group_by(year) %>%
  summarize(male = sum(male_jail_pop_rate, na.rm = TRUE))

sum_data <- left_join(female_sum, male_sum, by = "year") %>%
  pivot_longer(col= c("female", "male"),
               names_to = "Gender")

ggplot(sum_data) +
  geom_line(aes(x = year, y = value, group = Gender, colour = Gender)) +
  ggtitle("The Jail Rate by Gender Overtime") + 
  ylab("Jail Rate") + 
  xlab("Time(year)") +
  scale_x_continuous(breaks = round(seq(1990, 2018, by = 1))) +
  scale_y_continuous(labels =scales::comma)+
  theme(axis.text.x = element_text(angle = 90))
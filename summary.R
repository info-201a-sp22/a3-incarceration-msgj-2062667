library('dplyr')

prison_pop_data <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-jail-rates-1990.csv?raw=true", stringsAsFactors = FALSE)

report <- list()

data <- prison_pop_data %>% 
  mutate(location = paste0(county_name, ", ", state))

avg_jail_pop_rate_2018 <- data %>%
  filter(year == 2018) %>%
  group_by(location) %>% 
  summarize(mean_total_jail_pop_rate = mean(total_jail_pop_rate, na.rm = TRUE),
            mean_male_jail_pop_rate = mean(male_jail_pop_rate, na.rm = TRUE),
            mean_female_jail_pop_rate = mean(female_jail_pop_rate, na.rm = TRUE))

report$total_jail_pop_rate_2018 <- avg_jail_pop_rate_2018 %>% 
  summarize(mean_total = mean(mean_total_jail_pop_rate, na.rm = TRUE)) %>%
  pull(mean_total)

report$max_jail_pop_rate_2018 <- avg_jail_pop_rate_2018 %>%
  filter(mean_total_jail_pop_rate == max(mean_total_jail_pop_rate, na.rm = TRUE)) %>%
  pull(location)

report$male_jail_pop_rate_2018 <- avg_jail_pop_rate_2018 %>%
  summarise(mean_male = mean(mean_male_jail_pop_rate, na.rm = TRUE)) %>%
  pull(mean_male)

report$female_jail_pop_rate_2018 <- avg_jail_pop_rate_2018 %>%
  summarise(mean_female = mean(mean_female_jail_pop_rate, na.rm = TRUE)) %>%
  pull(mean_female)

avg_jail_pop_rate_1990 <- data %>%
  filter(year == 1990) %>%
  group_by(location) %>%
  summarize(mean_total_jail_pop_rate = mean(total_jail_pop_rate, na.rm = TRUE),
            mean_male_jail_pop_rate = mean(male_jail_pop_rate, na.rm = TRUE),
            mean_female_jail_pop_rate = mean(female_jail_pop_rate, na.rm = TRUE))

report$max_jail_pop_rate_1990 <- avg_jail_pop_rate_1990 %>% 
  filter(mean_total_jail_pop_rate == max(mean_total_jail_pop_rate, na.rm = TRUE)) %>%
  pull(location)

report$total_jail_pop_rate_1990 <- avg_jail_pop_rate_1990 %>% 
  summarize(mean_total = mean(mean_total_jail_pop_rate, na.rm = TRUE)) %>%
  pull(mean_total)
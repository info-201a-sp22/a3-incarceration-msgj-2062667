library(ggplot2)
library(gridExtra)

jail_pop_data <- read.csv('https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-jail-rates-1990.csv?raw=true', stringsAsFactors = FALSE)

# Chart 3: The relation between total jail rate and female/male jail rate.(side-to-side comparison)

gender_data <- jail_pop_data %>%
  group_by(year) %>%
  mutate(female = sum(female_jail_pop_rate, na.rm = TRUE)) %>% 
  mutate(male = sum(male_jail_pop_rate, na.rm = TRUE)) %>% 
  mutate(total = sum(total_jail_pop_rate, na.rm = TRUE)) %>% 
  select(county_name,female, male, total)

ggp1 <- ggplot(gender_data) +
  geom_point(aes(x = total, y = female), color="darkred") +
  ggtitle("The relationship between female jail rate and total jail rate ") +
  ylab("Female jail rate") +
  xlab("Total jail rate")

ggp2 <- ggplot(gender_data) +
  geom_point(aes(x = total, y = male), color="darkred") +
  ggtitle("The relationship between male jail rate and total jail rate") +
  ylab("Male jail rate") +
  xlab("Total jail rate")

grid.arrange(ggp1, ggp2, ncol = 2) 
#Rim Wolde
#INFO-201
#2/25/22
#Assignment 3 (incarceration)

#code
library("ggplot2")

incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

#states by year
state_by_year <- incarceration_trends %>% 
  group_by(year, state) %>% 
  summarize(state_total = sum(black_pop_15to64)) %>% 
  drop_na()

View(state_by_year)

#line graph
ggplot(state_by_year) +
  geom_line(
    mapping = aes(x = year, y = state_total, group = state, color = state)
  )

bar_year <- incarceration_trends %>% 
  filter(year == 2015 & state == "WA") %>% 
  mutate(Percentage = black_prison_pop / total_pop) %>% 
  arrange(desc(Percentage)) %>% 
  slice(1:10)

#my bar graph
ggplot(bar_year) +
  geom_col(
    mapping = aes(x = county_name, y = Percentage, fill = county_name) 
  ) +
  coord_flip()

d1 <- map_data("state")
d1$state <- sapply(d1$region, function(x) state.abb[grep(x,str_to_lower(state.name))])

map_data <- incarceration_trends %>%
  filter(year == 2015) %>%
  group_by(state) %>% 
  summarise(State_total = sum(black_jail_pop, na.rm = TRUE)) %>% 
  left_join(d1)

View(map_data("state"))

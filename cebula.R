library("tidyverse")
library("skimr")
library("lubridate")
library("readxl")
library("dplyr")
library("viridis")
install.packages("ggExtra")
library("gridExtra")
library("ggplot2")

cebula <- read.csv("cebula.csv")
View(cebula)
glimpse(cebula)

# before modifying column just I want to check how many factors are
# or if they are any mistakes in writing
distinct(cebula, state)
distinct(cebula, district)
distinct(cebula, market)
distinct(cebula, commodity)
distinct(cebula, variety)

cebula <- cebula %>%
  mutate(state = as.factor(state)) %>%
  mutate(district = as.factor(district)) %>%
  mutate(market = as.factor(market)) %>%
  mutate(commodity = as.factor(commodity)) %>%
  mutate(variety = as.factor(variety)) %>%
  mutate(arrival_date = dmy(arrival_date))
  
skim(cebula)
# we have:
# 107295 observations
# 22 states, 315 districts, 905 markets, 21 varieties of onion
# there are 6 states missing, Wikipedia tells they are 28
# no missing values
# data collected from 01.01.2020 to 16.09.2020
# median date is 08.05.2020 
# 260 unique values (dates), 260th day of 2020 was 16.09, so from every day in data set 
# we have information
# commodity is useless
# mean in max_price, min_price and modal_price is greater than median, 
# so their distributions are skewed to the right, 
# in all three cases the third quantile is about 8 times smaller than the fourth quantile
# so I suppose that onion in most cases is relatively cheap

# I'm adding info about population of states I'll take information from
# https://statisticstimes.com/demographics/india/indian-states-population.php.
states <- read_excel("states.xls") 

# Which type of onion is the most frequent in the data set?
# (excluding type "Other" and "Onion")
cebula %>%
  filter(variety != 'Other' & variety != 'Onion') %>%
  count(variety) %>%
  ggplot(aes(x = reorder(variety, desc(-n)), y = n )) + 
  geom_col() + 
  coord_flip() +
  labs(x = "Onion type", y = "How many items with this type we have")

cebula %>%
  filter(variety != 'Other' & variety != 'Onion') %>%
  select('state', 'district', 'market', 'variety') %>%
  distinct() %>%
  count(variety) %>%
  ggplot(aes(x = reorder(variety, desc(-n)), y = n )) + 
  geom_col() + 
  coord_flip() +
  labs(x = "Onion type", y = "In how many shops this type was available at least once")
# The most "common" is Red
# I would like to compare it in the table, just to see if the order is different.
onion_items <- cebula %>%
  filter(variety != 'Other' & variety != 'Onion') %>%
  count(variety) %>%
  arrange(desc(n))
 
onion_shops <- cebula %>%
  filter(variety != 'Other' & variety != 'Onion') %>%
  select('state', 'district', 'market', 'variety') %>%
  distinct() %>%
  count(variety) %>%
  arrange(desc(n))

comparison_1 <- cbind(onion_items, onion_shops) %>% view()
# We can see that for first six most popular varieties
# order is the same.
# For the rest orders differ, but it can be caused by small amount of items.

# What percentage of all items (arrivals in shops) are different varieties?
cebula %>%
  group_by(variety) %>%
  count() %>%
  ungroup() %>%
  mutate(perc = n / sum(n)) %>%
  select(-n) %>%
  arrange(desc(perc))

# how many of varieties we have per state
cebula %>%
  filter(variety=="Red" | variety =="Local" | variety=="1st Sort"
         | variety=="Nasik" | variety=="Big" | variety=="Small") %>%
  distinct(state, variety) %>%
  count(state) %>%
  arrange(desc(n))
# In Andhra Pradesh, Himachal Pradesh, Jharkhand, Karnataka , NCT of Delhi, Telangana 
# they sell only one type of onion

# How many types of markets are in each of these states?
cebula %>%
  filter(state == c('Andhra Pradesh', 'Goa', 'Nagaland', 'Telangana', 'Uttrakhand')) %>%
  group_by(state, market) %>%
  count() %>%
  View()

# I want plot showing
# proportions of 6 the most popular types of  varieties in each state
cebula %>%
  filter(variety=="Red" | variety =="Local" | variety=="1st Sort"
         | variety=="Nasik" | variety=="Big" | variety=="Small") %>%
  ggplot(aes(state, fill=variety)) + 
  geom_bar(position="fill") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_fill_manual(values = c(Red = "#874033", Local = "#a3d151", '1st Sort' = "#a349ca", Nasik = "#8dd2a5",
                               Big = "#cb4578", Small = "#5f723f"))
# observation: variety 'Big' is sold only in one state, but it's still one of the most popular

# plot illustrating daily numbers of arrivals for top 6 varieties
cols <- c("#5552fe", "#a8ff9f", "#ea0056", "#017da7", "#d98800", "#ffa48e")
cebula %>%
  filter(variety=="Red" | variety =="Local" | variety=="1st Sort"
         | variety=="Nasik" | variety=="Big" | variety=="Small") %>%
  count(variety, arrival_date) %>%
  ggplot(aes(arrival_date, n , col = variety)) +
  geom_smooth() + 
  scale_colour_manual(values = cols)
# amount of daily arrivals is the biggest for red onion throughout the whole time
# confidence interval for red onion is very big, but it doesn't change the fact
# amount of daily arrivals of red onion is several times higher than for other varieties

# In particular market we can find one type of onion or more?
cebula %>%
  group_by(market, variety) %>%
  summarise() %>%
  count(market) %>%
  arrange(desc(n))
# max 4 min ofc 1

# How many different markets do we have for each state?
cebula %>%
  select(state, market) %>%
  distinct() %>%
  group_by(state) %>%
  count() %>%
  arrange(n)
# max is 223 - Uttar Pradesh
# min is 1 - Goa and Nagaland

# Let's compare amount of different markets and population.
# Does bigger population means bigger variety of markets?
cebula %>%
  select(state, market) %>%
  distinct() %>%
  count(state) %>%
  left_join(states) %>%
  mutate(population = population / 1000000) %>%
  ggplot(mapping = aes(x = population, y = n)) + geom_point() +
  labs(x = "Population in mln", y = "Number of markets")
# The answer is: yes, the tendency is the more people, more various markets. 

# Which markets are the most common?
cebula %>%
  select(state, district, market) %>%
  distinct() %>%
  count(market) %>%
  arrange(desc(n))
# Only four markets appear in more than one district (precisely - in two districts)
# those four: Fatehabad, Hamirpur, Pratapgarh, Sirsa.
# It means that each market, apart those four, is only in one district.
# Or, every "chain" of markets, apart those four, is in only one district.
# By chain of markets I understand several markets with the same name, products and prices.

# I would like to check if data is collected equally for each state.
cebula <- cebula %>%
  mutate(month = month(arrival_date))

# I'm checking how many arrivals are per day.
cebula %>%
  count(state, district, market, arrival_date) %>%
  arrange(desc(n)) %>%
  view()
# No more than four. 
# I suppose it's because every type of onion is considered as different arrival,
# even if they arrived in the same day.
# It is consistent with the fact that in one market we can find
# max 4 types of onion (lines 128-135).

# I'm checking how amount of arrivals changes in time, I exclude September
cebula %>%
  filter(month != 9) %>%
  count(month) %>%
  ggplot(aes(x=month, y=n)) + geom_point()
# there's drop in March, April and August
# is it connected with prices?
# according to graph below it's not

# prices of onion in time
cols <- c("#e00029", "#39ff6b", "#3d0048")
cebula %>%
  group_by(arrival_date) %>%
  summarise(modal = median(modal_price),
            min = median(min_price),
            max = median(max_price)) %>%
  ggplot() +
  geom_smooth(mapping = aes(arrival_date, modal, col = "modal")) +
  geom_smooth(mapping = aes(arrival_date, min, col = "min")) +
  geom_smooth(mapping = aes(arrival_date, max, col = "max")) +
  labs(x = "Arrival_date", y = "Price") + 
  theme(legend.position = "bottom") +
  scale_colour_manual(values = cols)

# how price is changing for each variety?
ggplot(cebula, aes(arrival_date, modal_price, col=variety)) +
  geom_smooth()
# we don't see much 
# there's increase for Small onion

# plots of modal price and amount of arrivals throughout the year for
# seven the most popular varieties
cols <- c("#2c0d00", "#96d500", "#00379d", "#fff884", "#d585ff", "#007d68", "#ec006b")

g1 <- cebula %>%
  filter(variety=="Red" | variety=="Onion" | variety =="Local" | variety=="1st Sort"
         | variety=="Nasik" | variety=="Big" | variety=="Small") %>%
  ggplot(aes(arrival_date, modal_price, col=variety)) +
  geom_smooth() +
  scale_colour_manual(values = cols) +
  scale_y_log10() +
  labs(y = 'Modal price', x = 'Arrival date')

g2 <- cebula %>%
  filter(variety=="Red" | variety=="Onion" | variety =="Local" | variety=="1st Sort"
         | variety=="Nasik" | variety=="Big" | variety=="Small") %>%
  count(variety, arrival_date) %>%
  ggplot(aes(arrival_date, n , col = variety)) +
  geom_smooth() + 
  scale_colour_manual(values = cols) +
  scale_y_log10() +
  labs(y = 'Amount of arrivals', x = 'Arrival date')

grid.arrange(g1, g2)

# Small is the most expensive
# there is increase in the price of Small onion in April
# I want to check it
# I will investigate Small onion

# I'm checking if price of Small onion is dependent on state where is sold
cebula %>%
  filter(variety == "Small") %>%
  group_by(state, arrival_date) %>%
  summarise(modal = median(modal_price)) %>%
  ggplot(aes(arrival_date, modal, col = state)) +
  geom_point()
# this increase is caused by state Kerala
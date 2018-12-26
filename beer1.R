#import libraries
library(mice)
library(VIM)
library(dplyr)
library(choroplethr)
library(openintro)
library(choroplethrMaps)
library(ggplot2)
library(fiftystater)
library(colorplaner)

#Load Data
beers <- read.csv(file.choose(), header = T)
brew <- read.csv(file.choose(), header = T)
colnames(brew) <- c('brewery_id', 'brew_name', 'city', 'state')
beersmerged <- merge(beers, brew)
beersmerged <- as_data_frame(beersmerged)

newbr <- brew %>% group_by(state) %>% summarise(total = n())
colnames(newbr) <- c('region', 'value')
newbr$region <- trimws(newbr$region)
newbr$region <- abbr2state(newbr$region) 
newbr$region <- tolower(newbr$region)

newbeers <- beersmerged %>% group_by(state) %>% summarise(total = n())
colnames(newbeers) <- c('region', 'value')
newbeers$region <- trimws(newbeers$region)
newbeers$region <- abbr2state(newbeers$region) 
newbeers$region <- tolower(newbeers$region)

#Barplot of beers by state
newbeers1 <- newbeers
newbeers1$region <- state2abbr(newbeers1$region)
barplot(newbeers1$value, 
        names.arg = newbeers1$region, horiz = F, 
        col = "green", ylim = c(0, 300), 
        axisnames = T, las = 2,
        main = "Beers produced by state",
        xlab = "States", ylab = "Number of Beers")

#1st plot of beers by state
state_choropleth(newbeers, title = "Beers produced by State", legend = 'Number of Beers', num_colors = 9) 

#2nd plot of beers by state
ggplot(newbeers, aes(map_id = region)) + 
  geom_map(aes(fill = value), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) + 
  coord_map() + 
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) + 
  labs(x = "", y = "") + 
  theme(legend.position = "bottom",
        panel.background = element_blank()) +
  fifty_states_inset_boxes()

#Barplot of breweries by state
newbr1 <- newbr
newbr1$region <- state2abbr(newbr1$region)
barplot(newbr1$value, 
        names.arg = newbr1$region, horiz = F, 
        col = "Orange", ylim = c(0, 50), 
        axisnames = T, las = 2,
        main = "Breweries by state",
        xlab = "States", ylab = "Number of Breweries")

#1st plot of breweries by state
state_choropleth(newbr, title = "Breweries by State", legend = 'Number of Breweries') 

#2nd plot of breweries by state
ggplot(newbr, aes(map_id = region)) + 
  geom_map(aes(fill = value), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) + 
  coord_map() + 
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) + 
  labs(x = "", y = "") + 
  theme(legend.position = "bottom",
        panel.background = element_blank()) +
  fifty_states_inset_boxes()

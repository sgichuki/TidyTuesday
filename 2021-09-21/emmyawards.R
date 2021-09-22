library(tidyverse)
library(waffle)
library(dplyr)
library(RColorBrewer)
library(magick)

#load data
nominees <- read.csv("~/GitHub/TidyTuesday/2021-09-21/nominees.csv")

#convert columns to factor 
cols2factor <-c('type','distributor','producer')
nominees[,cols2factor]<-lapply(nominees[,cols2factor],as.factor)

#Winners by network
winners <- nominees %>%
  select(type,category,title,producer,distributor,year) %>%
  filter(distributor %in% c("ABC","NBC","CBS","FOX","FX Networks","Netflix","Hulu","Showtime","HBO","HBO Max","Disney+")) %>%
  filter(type == "Winner") %>%
  filter(year >= 2019) %>%
  group_by(distributor,year) %>%
  count(title)


# Color blind friendly palette with grey: first 8 colors found online, 
# had to add 3 more to fit data
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
          "#744D88","#DA38CD","#4D72FF")

#Read in the Emmy awards logo 
emmylogo <- image_read("~/GitHub/TidyTuesday/2021-09-21/emmyawardlogo.png")

#Create your base plot
myplot = ggplot(data = winners, aes(fill = distributor,values = n)) + 
  geom_waffle(color = 'white',size = 0.25, n_rows = 5, flip = TRUE)+
    scale_fill_manual(values = cbp1) +
    facet_wrap(~year) +
    scale_x_discrete() +
    labs(title = "Emmy award wins by network 2019 - 2020", 
    caption = "Data from emmys.com, viz:Susan G.twitter:@swarau")+
    theme(plot.title = element_text(color = "#3F5079", hjust = 0.5))+
    theme(legend.title = element_blank())
   

myplot #base plot here 

#this code adds the logo on top of your base plot, x and y values position the logo
grid::grid.raster(emmylogo, x = 0.95, y = 0.28, just = c('right', 'top'), width = unit(0.85, 'inches'))





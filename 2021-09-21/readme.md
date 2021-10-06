# Emmy awards 

## Introduction: 
An Emmy Award, or simply Emmy, is a trophy presented at one of the numerous annual American events or competitions that each recognize achievements in a particular sector of the television industry. The Emmy is considered one of the four major entertainment awards in the United States, the others being the Grammy (for music), the Oscar (Academy Award) (for film), and the Tony (for theatre). The two events that receive the most media coverage are the Primetime Emmy Awards and the Daytime Emmy Awards, which recognize outstanding work in American primetime and daytime entertainment programming, respectively. 

I wanted to experiment with waffle plot for this data set. Waffle plots, sometimes referred to as square pie charts, can be useful in presenting parts of a whole for categorical data. 

The number of distribution(mainly TV) networks represented at the Emmy awards are 199 going by the 'distributor' column in this dataset. For purposes of clarity, I picked out a few major TV and streaming platforms: 

````
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

````



![](emmywaffleplot.png)

library(tidyverse)
library(lubridate)
library(cowplot)
library(ggpubr)

earn <-read.csv("~/GitHub/TidyTuesday/2021-02-23/earn.csv")
#earn[,2:5] <- lapply(earn[,2:5],as.factor)
earn$year <- as.Date(as.character(earn$year),format = "%Y")

#Average weekly earnings for each racial grouping
avgwage <- earn %>%
  group_by(year,race)%>%
  summarize(averagewage = mean(median_weekly_earn))

ggplot(avgwage,aes(x=year,y=averagewage))+
  geom_step(aes(colour=race),size=1.5)+ylab("")+
  ggtitle("Average weekly earnings for U.S workers")+  
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(n.breaks=10,labels=scales::dollar_format())+
  theme(legend.title = element_blank())+
  theme(legend.position="top")

earn_df2<- earn %>% 
  select(sex,race,ethnic_origin,year,median_weekly_earn) %>% 
  mutate(race_unclass = case_when(race == "Black or African American" ~ "Black",
                          race == "All Races" & ethnic_origin == "Hispanic or Latino" ~ "Hispanic\nor Latino",
                          TRUE ~ race))

wagesbygender<-earn_df2 %>% 
  mutate(race_unclass = factor(race_unclass, levels = c("Black","Hispanic\nor Latino","White","Asian"))) %>%
  group_by(race_unclass,sex,year) %>%
  summarize(earn = mean(median_weekly_earn)) %>%
  filter(!sex == "Both Sexes") %>%
  pivot_wider(names_from = sex, values_from =earn) %>%
  mutate(diff= Men-Women)

wagesbygender<-na.omit(wagesbygender)

wagediffplot=ggplot(wagesbygender,aes(x=year,y=diff,fill=race_unclass,color=race_unclass))+
  geom_line()+ theme_classic()+
  geom_area()+facet_grid(~race_unclass)+
  scale_y_continuous(n.breaks=10,labels=scales::dollar_format())+
  theme(axis.text.x = element_text(angle=50, hjust=1))+
  ylab("")+theme(legend.position='none')+
  
#scale_x_continuous(labels=c(2010,2012,2014,2016,2018,2020),
 #                    breaks = c(2010,2012,2014,2016,2018,2020))
  
ggsave("wagediffplot.png",width = 297,height = 210,units = c("mm"),dpi = 300)

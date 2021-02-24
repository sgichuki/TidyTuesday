library(tidyverse)
library(lubridate)
library(ggthemes)
#Load data 
earn <-read.csv("~/GitHub/TidyTuesday/2021-02-23/earn.csv")

#Change variable types to factor and change year to Date format(lubridate)
earn[,2:5] <- lapply(earn[,2:5],as.factor)
earn$year <- as.Date(as.character(earn$year), format = "%Y")

#Average weekly earnings for each racial grouping
avgwage <- earn %>%
  group_by(year,race)%>%
  summarize(averagewage = mean(median_weekly_earn))


#Make the plot 
ggplot(avgwage,aes(x=year,y=averagewage))+
  geom_step(aes(colour=race),size=1.5,arrow = arrow())+ylab("")+xlab("")+
  ggtitle("U.S. workers average weekly earnings")+  
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(n.breaks=10,labels=scales::dollar_format())+
  theme(legend.title = element_blank())+
  theme(legend.position="top")+
  theme(panel.background = element_rect(fill = "floralwhite",
                                        colour = "black",size = 0.5, 
                                        linetype = "solid"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_line(colour = "gray88"))+
        theme(plot.background = element_rect(fill = "oldlace"))+
        theme(legend.background = element_rect(fill="oldlace", 
                                         size=0.5, linetype="solid"))

ggsave("US_workerwage_plot.png",width = 297,height = 210,units = c("mm"),dpi = 300)




  
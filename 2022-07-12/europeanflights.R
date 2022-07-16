library(tidyverse)
library(scales)
library(showtext)

font_add_google("Sacramento", "sacramento")
font_add_google("Roboto", "roboto")

#Showtext will be automatically invoked when needed
showtext_auto()

#load data 
flights <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv')

flights <- janitor::clean_names(flights)

df <- flights %>%
  group_by(apt_name,state_name,year,month_num)%>%
  summarize(totalflights = sum(flt_tot_1))%>%
  filter(state_name == "France" & apt_name == "Paris-Charles-de-Gaulle")%>%
  ungroup() %>% 
  unite(col = "yearmonth", year, month_num, remove = FALSE)

ggplot(df)+
  geom_line(aes(x = yearmonth, y = totalflights, group = factor(year)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab(" ") + ylab(" Total flights")+
  scale_x_discrete(
    breaks = paste0(2016:2022, "_01"),
    labels = paste0(2016:2022, ", Jan.")
  )

df1 <- flights %>%
  group_by(apt_name,state_name,year,month_num)%>%
  summarize(totalflights = sum(flt_tot_1))%>%
  filter(state_name == "Netherlands")%>%
  ungroup() %>% 
  unite(col = "yearmonth", year, month_num, remove = FALSE)

ggplot(df1)+
  geom_line(aes(x = yearmonth, y = totalflights, group = factor(apt_name)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab(" ") + ylab(" Total flights")+
  scale_x_discrete(
    breaks = paste0(2016:2022, "_01"),
    labels = paste0(2016:2022, ", Jan.")
  )

df3 <- flights %>%
  group_by(apt_name,state_name,year,month_num)%>%
  summarize(totalflights = sum(flt_tot_1))%>%
  filter(apt_name == "Paris-Charles-de-Gaulle" | apt_name == "Amsterdam - Schiphol" |apt_name == "Frankfurt"| apt_name == "London - Heathrow")%>%
  ungroup() %>% 
  unite(col = "yearmonth", year, month_num, remove = FALSE)

ggplot(df3)+
  geom_line(aes(x = yearmonth, y = totalflights, group = factor(apt_name), color = apt_name),lwd = 0.8)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab(" ") + ylab(" Total flights")+
  labs(title = "Total flight numbers in Europe's major airports (2016-2022)",
      subtitle ="the numbers are the sum of arrivals and departures, i.e. IFR flight movements",
      caption = "#TidyTuesday,July 12 2022|Data: Eurocontrol|viz: Susan Gichuki,@swarau")+
  geom_vline(xintercept ="2020_04",linetype = "dotted",lwd = 0.8,color = "#4D4637")+
  geom_text(x = "2020_03",label = "COVID lockdown", y = 34000,angle = 90, size = 14)+
  scale_x_discrete(
    breaks = paste0(2016:2022, "_01"),
    labels = paste0(2016:2022, ",Jan")
  )+
  theme(legend.title = element_blank(),
        legend.background = element_rect("#FFF9EC"),
        legend.text = element_text(size = 32),
        plot.background = element_rect(fill = "#FFF9EC"),
        plot.title = element_text(family = "sacramento", size = 64,face = "bold",color = "#C0564A"),
        plot.subtitle = element_text(family = "sacramento", size = 48),
        panel.background = element_rect(fill = "#FFF9EC"),
        axis.line.y.left = element_line(colour = "grey"),
        axis.line.x.bottom = element_line(colour = "grey"),
        axis.text = element_text(family = "roboto",size = 36),
        axis.title = element_text(family = "roboto",size = 36),
        plot.caption.position = "plot")

#Save the plot as a PNG file  
ggsave(last_plot(),file = "europeflights.png",width = 297,height = 210,units = c("mm"),dpi = 300)

  
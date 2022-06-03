library(tidyverse)
library(ggbump)
library(scales)
library(showtext)

font_add_google("Outfit", "outfit")
font_add_google("Roboto", "roboto")

#Showtext will be automatically invoked when needed
showtext_auto()

#load data
poll <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/poll.csv')
reputation <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/reputation.csv')

reputation$industry <- as.factor(reputation$industry)
poll$industry <- as.factor(poll$industry)

automotive <- 
  poll %>% 
  mutate(year = 2022) %>% 
  distinct(company, industry, year, rank = `2022_rank`, rq = `2022_rq`) %>% 
  bind_rows(select(poll, company, industry, year, rank, rq)) %>% 
  arrange(company, year) %>% 
  group_by(company) %>% 
  mutate(change = rank - lag(rank, default = NA)) %>% 
  ungroup() %>% 
  filter(industry == "Automotive")

#remove the rows with Stellantis and Subaru because the data has too many NA values
automotive<-automotive[!(automotive$company=="Stellantis" | automotive$company=="Subaru"),]

mysubtitle <- str_wrap("The reputation of car companies from 2017-2022 as assessed by the Axios/Harris poll 100 a survey of 33,096 Americans.",140)

automotive %>% 
  filter(!is.na(rank)) %>% 
  ggplot(aes(year, rank, col = company)) +
  geom_bump(size = 3)+
  geom_point(aes(x=year, y=rank, color=company),shape = 21, size = 8, stroke = 1.25, fill = "white")+
  geom_text(aes(label = rank), size = 12, family = "roboto") +
  geom_text(data = automotive %>% filter(year == min(year)), aes(x = year - .05, y=rank, label = company), size = 9, hjust = 1) +
  geom_text(data = automotive %>% filter(year == max(year)), aes(x = year + .05, y=rank, label = company), size = 9, hjust = 0) +
  coord_cartesian(clip = "off")+ xlab(" ")+
  labs(title = "Automotive brand perception in the U.S:",
       subtitle = mysubtitle,
       caption = "#TidyTuesday,May 31 2022|Data: Axios and Harris Poll| viz: Susan Gichuki, @swarau")+
  scale_x_continuous(position = "top")+
  theme(legend.position = "none",
        axis.text = element_text(size = 38),
        axis.title = element_text(size = 34),
        axis.line.x.top = element_line(colour = "white"),
        plot.title = element_text(size = 64,family = "outfit",hjust = 0.5),
        plot.subtitle = element_text(size = 42,family = "roboto",hjust = 0.5),
        plot.caption = element_text(size = 32,family = "roboto"),
        panel.background = element_rect(fill = "#F0EDF6"),
        plot.background = element_rect(fill = "#F0EDF6"),
        panel.grid.major = element_blank())

#Save the plot as a PNG file  
ggsave(last_plot(),file = "companyreputation.png",width = 297,height = 210,units = c("mm"),dpi = 300)

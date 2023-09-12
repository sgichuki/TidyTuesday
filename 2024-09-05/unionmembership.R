library(tidyverse)
library(geofacet)

demographics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/demographics.csv')
wages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/wages.csv')
states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/states.csv')

states$sector <- factor(states$sector)
states$members <- numeric(states$members)

unionrep <- states %>%
  filter(sector == "Public" | sector == "Private") %>%
  select(year,state,state_abbreviation,sector,p_members)

plot1 = ggplot(unionrep) + geom_area(aes(x = year, y = p_members,fill = sector)) +
  facet_geo(vars(state))+
  
  scale_x_continuous(breaks = c(1983,2022), expand = c(0,0))+
                       
  scale_y_continuous(labels = scales::percent)+
    
  scale_fill_manual(values = c("#C3170B", "#4C8076", "#5ab4ac"))+
    
  labs(title = "Union Membership in the US", x = " ", y = " ",
         subtitle = "There are more public sector workers in unions compared to the private sector.",
         caption = "TidyTuesday: 5 Sept 2023 | Data source:Unionstats.com | viz:Susan Gichuki") +
    
  theme(legend.position = c(0.026,0.97),
        strip.background = element_rect(color="gray31", fill="white")+
  coord_cartesian(clip = "off"))

ggsave(plot1,file = "union_rep_by_state.png",width = 297,height = 210,units = c("mm"),dpi = 300)

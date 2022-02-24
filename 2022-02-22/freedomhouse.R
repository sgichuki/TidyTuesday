library(tidyverse)
library(janitor)
library(RColorBrewer)
library(geofacet)

freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')

freedom <- janitor::clean_names(freedom)

#african countries
africa <- freedom %>%
  filter(region_name == 'Africa') %>%
  mutate(country = recode(country,
                        "CÃfÂ´te dÃ¢â,¬â"¢Ivoire" = "Côte d'Ivoire",
                        "Sao Tome and Principe" = "São Tomé and Principe",
                        "United Republic of Tanzania" = "Tanzania",
                        "Congo" = "Republic of the Congo"))+

africa$country <- as.factor(africa$country)

#use the mean of the CIVIL LIBERTIES SCORE as the mid point of the scale
mid <- mean(africa$cl)

ggplot(africa) +
  geom_line(aes(x = year, y = cl,color = cl), size = 1) +
  
  #use geofacets
  facet_geo(~country,grid = africa_countries_grid1)+
  
  #change the font size of facet titles
  theme(strip.text.x = element_text(size = 5.5))+
  
  #color palette for the CL score
  scale_color_gradient2(midpoint = mid, low = "blue", mid = "grey",
                        high = "red", space = "Lab" )+
  
  #insert the labels for the plot 
  labs(title = "The evolution of civil liberties in African countries(1995-2020)", 
       subtitle = "Score of 1(high degree of freedom) to 7(low degree of freedom).\nMean of the civil liberties score is the scale mid-point",
       caption = "#TidyTuesday 22/02/2022| Data:Freedom House |@swarau-Susan G", 
       x = "", 
       y = "")+

  #control axis text label sizes and center plot title and subtitles
  theme(axis.text = element_text(size = 10))+
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle=element_text(hjust=0.5))+
  
  #change background color for main panel
  theme(plot.background = element_rect(fill = "#E9C9AC"))+
  
  #limits for the x and y scales
  scale_y_continuous(limits = c(0, 8), breaks = c(0, 4, 8))+
  scale_x_continuous(limits = c(1995, 2020), breaks = c(2000, 2020))+
  
  #match legend background to main panel background color
  theme(legend.background = element_rect(fill="#E9C9AC", 
                                         size=0.5, linetype="solid"))+
  theme(legend.position="right")+
  
  #insert a custom name for the legend title
  guides(col = guide_colourbar(title = "civil\nliberty\nscore"))

ggsave("africa-civil-liberties.png",width = 297,height = 210,units = c("mm"),dpi = 300)


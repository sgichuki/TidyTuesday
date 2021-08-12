library(tidyverse)
library(RColorBrewer)
library(scales)

investment <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/investment.csv')
chain_investment <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/chain_investment.csv')
ipd <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/ipd.csv')

#convert to factors 
investment$meta_cat <- as.factor(investment$meta_cat)
investment$category <- as.factor(investment$category)

#subset 
electricpower <- investment %>%
  filter(meta_cat == "Electric power")

electricpowertrends = ggplot(electricpower,aes(x = year, y = gross_inv)) + 
  geom_line(aes(colour = category)) +
  scale_color_brewer(palette = "Dark2") +
  xlab(" ") + ylab("Gross investment") +
  theme_bw() + theme(legend.position = "bottom")+
  #format plot background
  theme(plot.background = element_rect(fill = "#D0F5FF"))+
  labs(title = "Trends in U.S. electric power investment 1947 - 2017",
       caption = "Data: Bureau of Economic Analysis|TidyTuesday 10 août 2021|Viz by Susan G.",) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title = element_text(color = "#008A5F"))+
  annotate("text", x = 1995, y = 32990, label = "Steady increase in wind and solar\n investment since the 2000s")+
  geom_segment(aes(x = 2001, y = 30000, xend = 2007, yend = 14000),
             curvature = 0,color = "black", size = 0.7,
             arrow = arrow(length = unit(0.03, "npc"),
             type = "closed",
             ends = "last"))

#Save the plot
ggsave("electricpowertrends.png",width = 297,height = 210,units = c("mm"),dpi = 300)

#Calculate changes in yearly investment in wind and solar power
overall_yearly <-electricpower %>%
  mutate(
    YoY = (gross_inv - lag(gross_inv)) / lag(gross_inv))

YoYchangeplot = ggplot(overall_yearly, aes(x = year, y = YoY, fill = category)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent)+
  theme(legend.position = "bottom") +
  xlim(2000,2017)+
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_rect(fill = "#E3E8F1"),
    # Add axis line
    axis.line = element_line(colour = "grey"))+
  ylab("% change yearly") + xlab(" ")+
  labs(title = "Yearly percent change in electric power investment", 
       caption = "Data: Bureau of Economic Analysis|TidyTuesday 10 août 2021|Viz by Susan G.") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title = element_text(color = "#1E857D"))

#Save the plot
ggsave("YoYchangeplot.png",width = 297,height = 210,units = c("mm"),dpi = 300)





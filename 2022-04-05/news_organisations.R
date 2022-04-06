library(tidyverse)
library(patchwork)
library(RColorBrewer)
library(showtext)

# Load fonts ----
font_add_google(name = "Source Code Pro", family = "Source Code Pro")
font_add_google(name = "Raleway", family = "Raleway")
showtext_auto()


#load data 
news_orgs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-05/news_orgs.csv')

canadanews <- news_orgs %>%
  filter(factor(country) == "Canada")

#filter data to see publications by year 
pubyear <- canadanews %>%
  group_by(year_founded) %>%
  count(publication_name)%>%
  summarise(sum(n))%>%
  na.omit()

names(pubyear) <- c("year","sum")

plot1 = ggplot(pubyear) +
  geom_area(mapping = aes(x = year, y = sum),fill = "#8EB3A2")+
  annotate("text", x = 2005.5, y = 7.0, 
           label = "Publications founded per year", 
           lineheight = 0.7, 
           size = 15,
           color = "#005A26")+
  
  xlab(" ")+ ylab(" ")+
  ylim(0,8)+
  theme(panel.background = element_rect(fill = "#F2FEDC"),
        plot.background = element_rect(fill = "#F2FEDC"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(family = "Raleway",size = 36))

#convert these columns to factor
canadanews$state <- as.factor(canadanews$state)
canadanews$tax_status_current <- as.factor(canadanews$tax_status_current)
  

#filter data to aggregate publications for each province and current tax status
df1 <- canadanews %>%
  select(publication_name,city, state, country,tax_status_current,year_founded)%>%
  group_by(tax_status_current,state)%>%
  count(publication_name)%>%
  summarise(sum(n))%>%
  na.omit()

names(df1) <- c("tax_status_current","state","sum")

plot2 = ggplot(data = df1,mapping = aes(x = state, y = sum, fill = tax_status_current)) +
  geom_col(width = 0.7)+
  xlab("Provinces") + ylab("No. of digital publications") +
  labs(fill = "Current tax status",
       title = "Digital news publications in Canada",
       subtitle = "Most digital news organisations are for-profit,mainly based in Ontario.",
       caption = "#TidyTuesday April 5 2022|Data:Project Oasis|viz:Susan Gichuki,@swarau") +
  scale_fill_brewer(palette = "Dark2") +
  
  theme(panel.background = element_rect(fill = "#F2FEDC"),
        plot.background = element_rect(fill = "#F2FEDC"),
        plot.caption = element_text(family = "Raleway",size = 32),
        plot.caption.position = "plot",
        plot.subtitle = element_text(family = "Source Code Pro", size = 42),
        plot.title = element_text(family = "Source Code Pro", face = "bold",size = 72,hjust = 0),
        axis.title = element_text(family = "Raleway", size = 40),
        axis.text = element_text(family = "Raleway", size = 40),
        legend.title = element_text(family = "Raleway",size = 36),
        legend.text = element_text(family = "Raleway", size = 32),
        legend.background = element_rect(fill = "#F2FEDC"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"))
        

#combine the plots using patchwork package
digpubplot = plot2 + inset_element(plot1, left = 0.01,bottom = 0.6, right = 0.65, top = 1)+
  plot_layout(guides = "collect")

#Save the plot as a PNG file  
ggsave(digpubplot, file = "digpubplot.png",width = 297,height = 210,units = c("mm"),dpi = 300)


 



  

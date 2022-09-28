library(tidyverse)
library(MetBrewer)
library(ggrepel)

artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-27/artists.csv')

df1 <- artists %>% 
  group_by(type) %>% 
  summarise(total = sum(artists_n, na.rm = TRUE))

#Create a donut chart - adapting ggplot to create this chart since there is no
#specific function for this

# Compute percentages
df1$fraction = df1$total / sum(df1$total)

# Compute the cumulative percentages (top of each rectangle)
df1$ymax = cumsum(df1$fraction)

# Compute the bottom of each rectangle
df1$ymin = c(0, head(df1$ymax, n=-1))

# Compute label position
df1$labelPosition <- (df1$ymax + df1$ymin) / 2

# Compute a good label
df1$label <- paste0(df1$type, "\n total: ", df1$total)

ggplot(df1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=type)) +
  geom_rect() + #creates stacked rectangles
  coord_polar(theta="y")+ #turns the rectangles into a pie chart
  xlim(c(2, 4)) + #adds the hole in the middle 
  #If xlim left boundary is big, no empty circle. You get a pie chart
  #If xlim is low, the ring becomes thinner.
  
  geom_label_repel(x=3.6, aes(y=labelPosition, label=label,check_overlap = TRUE,max.overlaps = Inf),color = "white", size=3)+
  labs(title = "Artists in the US workforce",
       subtitle = "Designers make up the largest share of artists",
       caption = "#TidyTuesday Sep 27 2022|Data:arts.gov by way of Data is Plural|viz:Susan Gichuki,@swarau ")+
  scale_fill_manual(values=met.brewer("Renoir", 13))+
  theme_void()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5,face = "bold",size = 20),
        plot.subtitle = element_text(hjust = 0.5,face = "bold", size = 16),
        plot.background = element_rect(fill = "white",color= "white"))
  
#Save the plot
ggsave(
  filename = "artists.png",
  device = "png",
  dpi = "retina",
  height = 8,
  width = 6)


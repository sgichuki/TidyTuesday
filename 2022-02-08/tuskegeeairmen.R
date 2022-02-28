library(tidyverse)
library(readr)
library(showtext)

#Loading fonts
font_add_google("Ultra", "Ultra")
font_add_google("Roboto", "Roboto")

showtext_auto()

#Load data
airmen <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv')

#Total airmen per state
airmenbystate <- airmen %>%
  group_by(state) %>%
  count()

#remove missing values from the dataframe
airmenbystate <- na.omit(airmenbystate)

#Plot
tuskegee_airmen = ggplot(airmenbystate,aes(x = state, y = n, fill = n))+
  
  # Make custom panel grid
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0:3) * 35),
    color = "lightgrey"
  ) + 
  
  geom_bar(stat = "identity")+ 
 

  #remove the legend, you don't need it for this case
  #theme(legend.position = "none") +
  coord_polar()+

  ## Scale y axis so bars don't start in the center
  scale_y_continuous(
  limits = c(-20,110),
  expand = c(0,0),
  breaks = c(0,30,60,90,110))+
  
  
  # New fill and legend title for number of tracks per region
  scale_fill_gradientn(
    "No. of airmen",
    colours = c("#F8B195","#ED9495","#D87C9C","#B56CA6","#8262AD","#2D5DAB")
  ) +
  # Make the guide for the fill discrete
  guides(
    fill = guide_colorsteps(
      barwidth = 15, barheight = .5, title.position = "top", title.hjust = .5
    )
  ) +
  theme(
    # Remove axis ticks and text
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    # Use gray text for the state initials/codes
    axis.text.x = element_text(color = "gray12", size = 9),
    # Move the legend to the bottom
    legend.position = "bottom",
  )   +

  # Annotate custom scale inside plot
  annotate(
    x = 11, 
    y = 36, 
    label = "35", 
    geom = "text", 
    color = "mediumvioletred", 
    family = "Roboto",
    size = 6
  ) +
  annotate(
    x = 11, 
    y = 71, 
    label = "70", 
    geom = "text", 
    color = "mediumvioletred", 
    family = "Roboto",
    size = 6
  ) +
  
  annotate(
    x = 11, 
    y =106, 
    label = "105", 
    geom = "text", 
    color = "mediumvioletred", 
    family = "Roboto",
    size = 6
    
  )+
  
  #change background color for main panel
  #theme(plot.background = element_rect(fill = "#E9C9AC"))+
  
  #match legend background to main panel background color
  #theme(legend.background = element_rect(fill="#E9C9AC", 
                                         #size=0.5, linetype="solid"))+
  
  labs(title = "Tuskegee airmen's home states",
       caption = "TidyTuesday 8 Feb 2022\nData: Commemorative Air Force(CAF)|viz:Susan G.@swarau")+
  
  
  
  theme(text = element_text(family = "Ultra", color = "black"), 
        axis.title = element_blank(), 
        axis.text.x = element_text(family = "Roboto",color = "black", size = 16), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),  
        panel.background = element_rect(fill = "#E9C9AC", color = "#E9C9AC"), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(colour = "#B9A89A"),
        plot.background = element_rect(fill = "#E9C9AC"), #old color: #E9C9AC
        legend.title = element_text(family = "Roboto", size = 14), 
        legend.text = element_text(family = "Roboto", size = 14),
        legend.background = element_rect(fill="#E9C9AC", 
                                         size=0.5, linetype ="solid"),
        plot.title = element_text(hjust = 0.5, size = 24),
        plot.caption = element_text(family = "Roboto", size = 12))

#Save the plot as a PNG file  
ggsave(tuskegee_airmen, file = "tuskegee_airmen.png", width = 5, height = 6.8, dpi = 300)



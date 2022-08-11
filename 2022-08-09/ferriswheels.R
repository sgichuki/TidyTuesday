library(tidyverse)
library(ggrepel)
library(RColorBrewer)
library(patchwork)
library(showtext)
library(scales)

font_add_google("Oleo Script","oleo")
font_add_google("Fredericka the Great","fredericka")
showtext_auto()

#load data 
wheels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-09/wheels.csv')

wheels <-janitor::clean_names(wheels)

#convert to factors
wheels$name <- as.factor(wheels$name)
wheels$country <-as.factor(wheels$country)
wheels$status <- as.factor(wheels$status)

plot1 = ggplot(wheels)+
  geom_point(aes(x = opened, y = seating_capacity, size = number_of_cabins, color = status),alpha = 0.8)+
  scale_size(range = c(2, 10))+
  geom_text_repel(data=wheels %>% filter(seating_capacity > 500), # Filter data first
            aes(x= opened, y = seating_capacity, label=name),max.overlaps = Inf,colour = "white",size = 14)+
  xlab("Date opened") + ylab("Seating capacity")+
  scale_x_date(breaks = pretty_breaks(n = 8))+
  labs(title = "Ferris Wheels: from the 1900's to present day",
       caption = "#TidyTuesday, August 9 2022|Data:{ferriswheels} package by Emil Hvitfeldt|viz:Susan Gichuki,@swarau",
       size = "No. of cabins")+
  scale_color_brewer(palette = "Set3")+
  theme(panel.background = element_rect(fill = "#525E7D"),
        plot.background = element_rect(fill = "#E4F0FF"),
        plot.subtitle = element_text(size = 38),
        plot.caption = element_text(size = 30),
        plot.caption.position = "plot",
        legend.background = element_rect(fill = "#E4F0FF"),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 32),
        panel.grid.major = element_line(linetype = "dotted"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(family = "oleo", size = 72,color = "#CF2779"),
        axis.title = element_text(size = 36),
        axis.text = element_text(size = 36))

#Define the colors you want
nb.cols <-17
mycolors <- colorRampPalette(brewer.pal(8, "Accent"))(nb.cols)

#remove row with missing value in height column
wheels <- wheels[-c(41),] 

plot2 = ggplot()+
  geom_col(data = wheels %>%
          filter(height > 328 & status == "Operating"),aes(y= height/3.281, x = reorder(name,height), fill = country))+
  scale_fill_manual(values =  mycolors)+
  xlab(" ")+ ylab(" Height(meters)")+
  labs(title = "How high does it go?",
       caption = "#TidyTuesday, August 9 2022|Data:{ferriswheels} package by Emil Hvitfeldt|viz:Susan Gichuki,@swarau")+
  annotate("text", x = 4, y = 160, label = "Ferris Wheels", size = 48,family = "fredericka", color = "#FFEECB")+
  #scale_x_continuous(position = "top")+
  theme(panel.background = element_rect(fill = "#525E7D"),
        plot.background = element_rect(fill = "#E4F0FF"),
        plot.title = element_text(family = "fredericka", size = 72),
        plot.caption = element_text(size = 34),
        legend.background = element_rect(fill = "#E4F0FF"),
        legend.title = element_blank(),
        legend.text = element_text(size = 32),
        legend.position = "top",
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", color  = "white"),
        plot.caption.position = "plot",
        axis.text = element_text(size = 32, face = "bold"),
        axis.title = element_text(size = 32),
        axis.text.x=element_text(angle=45, hjust=1))
    

#plot1 / plot2

#Save the plot as a PNG file  
ggsave(plot1,file = "ferriswheels1.png",width = 297,height = 210,units = c("mm"),dpi = 300)
ggsave(plot2,file = "ferriswheels2.png",width = 297,height = 210,units = c("mm"),dpi = 300)

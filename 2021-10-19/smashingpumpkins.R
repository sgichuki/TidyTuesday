library(tidyverse)
library(tidyr)
library(RColorBrewer)
library(patchwork)
library(showtext)


#Add font you will use
font_add_google("Henny Penny")
font_add_google("Montserrat")
showtext_auto()

pumpkins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv')

#Split the id column and mutate "type" column to show names of pumpkin varieties
#Types: F = "Field Pumpkin", P = "Giant Pumpkin", S = "Giant Squash", 
#W = "Giant Watermelon", L = "Long Gourd" 
#(length in inches, not weight in pounds), T = Tomato

pumpkins_df <- pumpkins %>% 
  separate(id, c("year", "type"), sep = "-") %>%
  mutate(type = case_when(type == "F" ~ "Field Pumpkin",
                          type == "P" ~ "Giant Pumpkin",
                          type == "S" ~ "Giant Squash",
                          type == "W" ~ "Giant Watermelon",
                          type == "L" ~ "Long Gourd",
                          type == "T" ~ "Tomato",
                          TRUE ~ "NA"))%>%
  mutate(weight_lbs = gsub(",", "", weight_lbs)) %>%
  mutate(across(c(weight_lbs, place), as.numeric)) %>%
  # Convert to kgs
  mutate(weight_kgs = weight_lbs * 0.45359237)


#Giant pumpkins
giantpumpkin <- pumpkins_df %>%
  filter(type %in% c("Giant Pumpkin","Giant Squash")) %>%
  group_by(year,country) 

#Medium pumpkins
mediumpumpkin <- pumpkins_df %>%
  filter(type %in% c("Field Pumpkin", "Giant Watermelon", "Long Gourd")) %>%
  group_by(year,country)

pumpkincolors <- c("#F1850A","#CC9A00","#F2E02B","#B68E00","#778F00",
                   "#298A12","#FF9354","#D04E01")

#Weight distributions for each type of pumpkin
p1 = ggplot(giantpumpkin,aes(x = year, y = weight_kgs,fill = type),na.rm = TRUE)+ 
  geom_violin()+xlab(" ")+ ylab(" weight(kgs)")+
  geom_boxplot(width=0.1, fill="white")+
  facet_wrap(~type)+
  theme(strip.background = element_rect(
      color="black", fill="#FFEACF", size=1.5, linetype="solid"))+
  theme(strip.text = element_text(
    size = 42, color = "black"))+
  labs(title = "Weight distribution of giant squash and pumpkins")+
  theme(legend.position = "none")+
  scale_fill_manual(values = pumpkincolors)+
  theme(plot.background = element_rect(fill = "#BAA89B" ))+
  theme(panel.background = element_rect(fill = "#BAA89B",color = "black",size = 1.0, linetype = "solid"))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(axis.text = element_text(color="white",size = 18),
        axis.title.y = element_text(family = "Montserrat",size = 36),
        title = element_text(family = "Henny Penny", size = 64,color = "white"))
  
 
p2 = ggplot(mediumpumpkin,aes(x = year, y = weight_kgs,fill = type),na.rm = TRUE)+ 
  geom_violin()+xlab(" ")+ ylab(" weight(kgs)")+
  geom_boxplot(width=0.1, fill="white")+
  facet_wrap(~type)+
  theme (strip.background = element_rect(color="black", fill="#FFEACF", size=1.0, linetype="solid"))+
  theme(strip.text = element_text(
    size = 42, color = "black"))+
  theme(legend.position="none")+
  scale_fill_manual(values = pumpkincolors)+
  theme(plot.background = element_rect(fill = "#BAA89B" ))+
  theme(panel.background = element_rect(fill = "#BAA89B",color = "black",size = 1.0, linetype = "solid"))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(axis.text = element_text(color="white",size = 22),
        axis.title = element_text(family = "Montserrat",size = 36,color = "white"))+
  theme(plot.caption = element_text(size = 32))+
  labs(caption = "Data: Great Pumpkin Commonwealth,viz: Susan G @swarau")

(p1/p2)

#Save the plot
ggsave("pumpkinplot.png",width = 297,height = 210,units = c("mm"),dpi = 300)


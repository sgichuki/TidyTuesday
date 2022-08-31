library(tidyverse)
library(MetBrewer)
library(ggrepel)
library(showtext)

font_add_google("Hepta Slab","heptaslab")

showtext_auto()

pell <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-30/pell.csv')

names(pell) <- c("state","award","recipients","institution","session","year")




ivyleague <-c("Brown University","Columbia University","Cornell University","Dartmouth College",
              "Harvard University","University of Pennsylvania","Princeton University","Yale University")

#Replace all characters occurrence in a string - to harmonize name for Columbia University
pell$institution <- gsub('Columbia University in the City of New y','Columbia University in the City of New York',pell$institution)
pell$institution <- gsub('Columbia University in the City of New York','Columbia University',pell$institution)

#Based on bestcolleges.com
#elite_hbcu <-c("Dillard University","Fisk University","Hampton University",
            #   "Howard University","Morehouse College","Spelman College","Tuskegee University")

#Based on US News 2022 ranking
top_hbcu <-c("Spelman College","Howard University","Xavier University of Louisiana","Hampton University",
             "Morehouse College","Tuskegee University","Florida A&M University",
             "North Carolina A&T","Fisk University","Claflin University","Delaware State University")


ivyleaguedesc <-str_wrap("The Ivy League consists of 8 private research universities, all located in the North Eastern United States.All Ivy League schools are highly competitive, admitting fewer than 1 in 10 applicants.Although expensive, they often provide generous financial aid packages", width = 75)

#Replace all characters occurrence in a string - to harmonize name for Columbia University
pell$institution <- gsub('Columbia University in the City of New y','Columbia University in the City of New York',pell$institution)
pell$institution <- gsub('Columbia University in the City of New York','Columbia University',pell$institution)

pell %>%
  filter(institution %in% ivyleague)%>%
  #mutate(label = if_else(year == max(year),as.character(institution),NA_character_)) 
  ggplot(mapping = aes(x = year, y = award)) +
  geom_point(aes(size = recipients, color = institution))+
  geom_line(aes(color = institution))+
  
  
  labs(title = "Pell Grant awards to the Ivy League",
       caption = "#TidyTuesday Aug 30 2022|Data:US Department of Education|data viz: Susan Gichuki,@swarau") +
  
  xlab("")+ylab("")+
  
  annotate("text", x = 2005.3, y = 11000000, label = ivyleaguedesc, colour = "#630000", size = 14, lineheight = 0.4)+
  
  scale_y_continuous(labels = scales::dollar)+
  theme(panel.background = element_rect("#E3F0FF"),
        plot.background = element_rect("#E3F0FF"),
        legend.background = element_rect("#E3F0FF"),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 32),
        axis.line = element_line("grey"),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 28),
        plot.title = element_text(family = "heptaslab", face = "bold", hjust = 0.5, size = 64, colour = "#425377"),
        plot.caption.position = "plot",
        plot.margin = margin(20,30,10,30))+
  scale_color_manual(values=met.brewer("Juarez", 8))

ggsave("ivyleague_pellgrants.png",width = 297,height = 210,units = c("mm"),dpi = 300)  

#Syntax: gsub(old,new,string)
pell$institution <-gsub('North Carolina A&t State Univ','North Carolina A&T',pell$institution)
pell$institution <-gsub('North Carolina Agricultural and Technical State University','North Carolina A&T',pell$institution)

pell$institution <- gsub('Xavier University of Louisana','Xavier University of Louisiana',pell$institution)

pellgrantdesc <- str_wrap("Federal Pell Grants are awarded to undergraduate students who display exceptional financial need and have not earned a bachelor's, graduate, or professional degree. In some cases,students enrolled in postbaccalaureate teacher certification programs receive Federal Pell Grants.",width = 60)

pell %>%
  filter(institution %in% top_hbcu)%>%
  
  ggplot(mapping = aes(x = year, y = award)) +
  geom_point(aes(size = recipients, color = institution))+
  geom_line(aes(color = institution))+
  
  labs(title = "Pell Grant awards to top HBCUs",
       subtitle = "Plot shows awards to the top Historically Black Colleges or Universities\naccording to the latest US News ranking(2022)",
       caption = "#TidyTuesday Aug 30 2022|Data:US Department of Education|data viz: Susan Gichuki,@swarau") +
  
  xlab("")+ylab("")+
  
  annotate("text", x = 2004.5, y = 28000000, label = pellgrantdesc, colour = "#630000")+
  
  scale_y_continuous(labels = scales::dollar)+
  theme(panel.background = element_rect("#FFF4EE"),
        plot.background = element_rect("#FFF4EE"),
        legend.background = element_rect("#FFF4EE"),
        legend.text = element_text(size = 12),
        axis.line = element_line("grey"),
        axis.text = element_text(size = 12),
        plot.title = element_text(family = "heptaslab", face = "bold", hjust = 0.5, size = 32, colour = "#EA6857"),
        plot.subtitle = element_text(size = 16),
        plot.caption.position = "plot",
        plot.margin = margin(20,30,10,30))+
  scale_color_manual(values=met.brewer("Klimt", 10))


showtext_opts(dpi = 300) 

ggsave("hbcu_pellgrants.png",width = 297,height = 210,units = c("mm"),dpi = 300)  

showtext_auto(FALSE)


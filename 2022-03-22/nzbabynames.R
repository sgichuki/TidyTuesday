library(tidyverse)
library(nzbabynames)
library(janitor)
library(RColorBrewer)
library(scico) #color blind friendly palette
library(ggrepel)
library(patchwork)
library(showtext)

font_add_google(name = "Vast Shadow", family = "Vast Shadow")
font_add_google(name = "Roboto", family = "Roboto")
showtext_auto()


#load data 
nzbabynames <-data.frame(nzbabynames)
nzbabynames <-janitor::clean_names(nzbabynames)

names <- c('sex','name')
nzbabynames[,names] <- lapply(nzbabynames[,names],factor)

#top names 
femalenz <- nzbabynames %>%
  filter(factor(sex) == "Female") %>%
  arrange(desc(year), desc(count)) %>%
  head(10) %>% pull(name)

malenz <- nzbabynames %>%
  filter(factor(sex) == "Male") %>%
  arrange(desc(year), desc(count)) %>%
  head(10) %>% pull(name)

top_nznames <- nzbabynames %>%
  arrange(desc(year), desc(count)) %>%
  head(10) %>% pull(name)

nzgirls <- nzbabynames %>%
  filter(year > 1999)%>%
  filter(name %in% femalenz) %>%
  group_by(year, name) %>%
  summarise(count = sum(count)) %>%
  ungroup()

plot1 = ggplot() + 
  geom_tile(data = nzgirls, mapping = aes(x = year, y = name,fill = count),
            color = "white",
            lwd = 0.4,
            linetype = 1) +
  scale_fill_scico(palette = "buda") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.y.left = element_line(color = "black"),
        axis.line.x.bottom = element_line(color = "black"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(family = "Roboto",size = 42,color = "black"),
        axis.title = element_text(family = "Roboto", size = 42, color = "black"),
        legend.text = element_text(family = "Roboto", size = 22, color = "black"),
        legend.title = element_text(family = "Roboto", size = 32, color = "black"))


nzboys <- nzbabynames %>%
  filter(year > 1999)%>%
  filter(name %in% malenz) %>%
  group_by(year, name) %>%
  summarise(count = sum(count)) %>%
  ungroup()

plot2 = ggplot()+ 
  geom_tile(data = nzboys, mapping = aes(x = year, y = name, fill = count),
            color = "white",
             lwd = 0.4,
             linetype = 1) +
  scale_fill_scico(palette = "bukavu") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.y.left = element_line(color = "black"),
        axis.line.x.bottom = element_line(color = "black"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(family = "Roboto",size = 42,color = "black"),
        axis.title = element_text(family = "Roboto", size = 42, color = "black"),
        legend.text = element_text(family = "Roboto", size = 22, color = "black"),
        legend.title = element_text(family = "Roboto", size = 32, color = "black"))


topnamesnz <- nzbabynames %>%
  filter(year > 2009)%>%
  filter(name %in% top_nznames) %>%
  group_by(year, name) %>%
  summarise(count = sum(count)) %>%
  ungroup()

plot3 = topnamesnz %>% #create a new column to hold the label to put at end of each line
  mutate(label = if_else(year == max(year),as.character(name),NA_character_)) %>%
  ggplot(mapping = aes(x = year, y = count, color = name)) + 
  geom_line(lwd = 1.3) +
  scale_color_brewer(palette = "Paired")+ xlab(" ")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5,family = "Vast Shadow",size = 52),
        axis.text = element_text(family = "Roboto",size = 48,color = "black"),
        axis.title = element_text(family = "Roboto", size = 42, color = "black"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major.y  = element_line(color = "white"),
        panel.grid.minor.x  = element_line(color = "white"),
        panel.grid.minor.y = element_line(color = "white"),
        panel.grid.major.x = element_line(color = "white"),
        panel.background = element_rect(fill = "#F7F3F7"))+
  
  geom_text_repel(aes(label = label),
                  size = 18,
                   nudge_x = 1,
                   na.rm = TRUE,
                  xlim = c(2020.8, NA),
                  hjust = 0,
                  segment.size = .7,
                  segment.alpha = .5,
                  segment.linetype = "dotted",
                  box.padding = .4,
                  segment.curvature = -0.1,
                  segment.ncp = 3,
                  segment.angle = 20)+
  coord_cartesian(clip = "off") +
  #labs(title = "Top baby names in New Zealand",
       #caption = "#TidyTuesday March 22 2022|Data: nzbabynames package|viz:Susan G@swarau") +
  scale_x_continuous(expand = c(0, 0),limits = c(2010, 2023.5))
  

patchwork = plot3 + plot1/plot2 +
  plot_annotation('Top baby names in New Zealand', 
                  caption = '#TidyTuesday March 22 2022|Data:GitHub-ekothe/nzbabynames|viz:Susan G,@swarau', 
                  theme = theme(plot.title = element_text(size = 72,family = "Vast Shadow", hjust = 0.5),
                                plot.caption = element_text(family = "Roboto",size = 36,color = "black")))

  
#Save the plot as a PNG file  
ggsave(patchwork, file = "nzbabynames.png",width = 297,height = 210,units = c("mm"),dpi = 300)



  
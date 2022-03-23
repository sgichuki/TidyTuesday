library(tidyverse)
library(nzbabynames)
library(janitor)
library(RColorBrewer)
library(scico) #color blind friendly palette
library(ggrepel)
library(data.table)
library(showtext)

font_add_google(name = "Pattaya", family = "Pattaya")
font_add_google(name = "Arimo", family = "Arimo")
showtext_auto()

#load data - fread is faster than read csv
babynames <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')

names <- c('sex','name')
babynames[,names] <- lapply(babynames[,names],factor)

susan <- babynames %>%
  filter(name == "Susan" & sex == "F")

ggplot(susan,aes(x = year, y = prop)) +
  geom_line()

celebnames <-c("Aaliyah","Mariah")

df1 <- babynames %>%
  filter(year > 1984 & sex == "F")%>%
  filter(name %in% celebnames) %>%
  group_by(year, name)
  

df1$name <- as.factor(df1$name)
df1$sex <- as.factor(df1$sex)


celebname = ggplot()+
  geom_line(data = df1, mapping = aes(x = year, y = prop, color = name),lwd = 1.7)+
  labs(title = "The female popstar effect:", 
       subtitle = "do chart-topping artists influence baby-naming trends?",
       caption = "#TidyTuesday March 22 2022|Data: Hadley Wickham babynames package|viz: Susan G @swarau") +
  scale_x_continuous(breaks = seq(1985,2022,2))+ xlab(" ") +
  scale_color_brewer(palette = "Accent") +
  
  annotate(
    geom = "curve", x = 2002, y = 0.0005, xend = 2000, yend = 0.00075, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  )+
  annotate("text", x = 2002, y = 0.0005,size = 11.5, label = "Aaliyah stars in the film\nRomeo Must Die",lineheight = 0.3)+
  geom_segment(aes(x = 1999, y = 0.0015, xend = 2001, yend = 0.0017),
               arrow = arrow(length = unit(2, "mm")))+
  annotate("text", x = 1996.5, y = 0.0015, size = 11.5,label = "Release of the self-titled album \nAaliyah(July 2001).\nAaliyah dies August 2001", lineheight = 0.3)+
  geom_segment(aes(x = 1988, y = 0.0007, xend = 1990, yend = 0.0005),
               arrow = arrow(length = unit(2, "mm")))+
  annotate("text", x = 1986.7, y = 0.00075,size = 11.5, label = "Debut album:\nMariah Carey\nJune 1990.", lineheight = 0.3)+
  annotate(
    geom = "curve", x = 1991, y = 0.0027, xend = 1993, yend = 0.0021, 
    curvature = -0.3, arrow = arrow(length = unit(2, "mm")))+ #curvature +ve value produces left hand curve;-ve value produces right hand curve
  
  annotate("text", x = 1989, y = 0.0028, size = 11.5, label = "Music Box released.Becoming one of the \nbest-selling albums of all time.\nOver 28 million copies sold worldwide.", lineheight = 0.3)+
  annotate(
    geom = "curve", x = 1998, y = 0.0029, xend = 1995, yend = 0.0025, 
    curvature = 0.35, arrow = arrow(length = unit(2, "mm"))
  ) +
  
  annotate("text", x = 2001.5, y = 0.0029,size = 11.5, label = "Daydream album release;the single Fantasy\ndebuts at no.1 on Billboard 100", lineheight = 0.3) +
  
  theme(plot.title = element_text(family = "Pattaya", hjust = 0.5, size = 72, color ="#F03800"),
        plot.background = element_rect(fill = "#FAF7FF" ),
        plot.caption = element_text(family = "Arimo", size = 32),
        plot.subtitle = element_text(family = "Pattaya", size = 48, hjust = 0.5, color ="#354E75"),
        panel.background = element_rect(fill = "#FAF7FF"),
        axis.text = element_text(family = "Arimo", size = 40),
        legend.title = element_blank(),
        legend.text = element_text(family = "Arimo", size = 42),
        axis.title.y = element_text(family = "Arimo", size = 42),
        axis.line.x.bottom = element_line(color = "black"),
        axis.line.y.left = element_line(color = "black"), 
        legend.background = element_rect(fill = "#FAF7FF"))

#Save the plot as a PNG file  
ggsave(celebname, file = "celebname.png",width = 297,height = 210,units = c("mm"),dpi = 300)

        
  
  

#top names 
topfemalenames <- babynames %>%
  filter(sex == "F") %>%
  arrange(desc(year), desc(prop)) %>%
  head(10) %>% pull(name)

topmalenames <- babynames %>%
  filter(sex == "M") %>%
  arrange(desc(year), desc(prop)) %>%
  head(10) %>% pull(name)

girls <- babynames %>%
  filter(year > 1999)%>%
  filter(name %in% topfemalenames) %>%
  group_by(year, name) %>%
  summarise(prop = sum(prop)) %>%
  ungroup()

ggplot(girls, aes(x = year, y = name, fill= prop)) + 
  geom_tile()

boys <- babynames %>%
  filter(year > 1999)%>%
  filter(name %in% topmalenames) %>%
  group_by(year, name) %>%
  summarise(prop = sum(prop)) %>%
  ungroup()

ggplot(boys, aes(x = year, y = name, fill= prop)) + 
  geom_tile()+
  scale_fill_scico(palette = "bukavu")


#top names 
femalenz <- nzbabynames %>%
  filter(factor(sex) == "Female") %>%
  arrange(desc(year), desc(count)) %>%
  head(10) %>% pull(name)


nzgirls <- nzbabynames %>%
  filter(year > 1949)%>%
  filter(name %in% femalenz) %>%
  group_by(year, name) %>%
  summarise(count = sum(count)) %>%
  ungroup()

ggplot(nzgirls, aes(x = year, y = name, fill= count)) + 
  geom_tile()

ggplot(data = nzgirls,aes(x = year, y = count, color = name),lwd = 1.2)+
  geom_line()+
  geom_label(aes(label = name),
                 xlim = c(2020.8, NA))

#topfemalenames <- babynames %>%
# filter(sex == "F")%>%
#  group_by(name,year) %>%
  #tally(prop) %>%
#  summarise(prop = sum(prop)) %>%
#  arrange(desc(n))%>%
#  slice(1:20) #can also use top_n from dplyr to filter top x values


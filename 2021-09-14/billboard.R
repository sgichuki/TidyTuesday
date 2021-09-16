library(tidyverse)
library(janitor)
library(lubridate)
library(ggrepel)

#Load data 
billboard <- read.csv("C:/Users/warau/Documents/GitHub/TidyTuesday/2021-09-14/billboard.csv")
audiofeatures <- read.csv("C:/Users/warau/Documents/GitHub/TidyTuesday/2021-09-14/audiofeatures.csv")

#clean column names
billboard <-clean_names(billboard)
audiofeatures <-clean_names(audiofeatures)

b1 = billboard %>% 
  mutate(date = mdy(week_id),year = year(date)) %>% select(-url)

#Create data frame with music from Destiny's child,Beyonce & Kelly Rowland music
destiny <- b1 %>%
  filter(performer == "Destiny's Child" | performer == "Beyonce" | performer == "Kelly Rowland")

#Pick out specific features from audiofeatures dataframe,
#Combine with the destiny's child songs data frame 
f1 = audiofeatures %>% select(song_id,loudness,danceability,valence,tempo)
df1 = destiny %>% left_join(f1, by = "song_id")

#Replace commas in decimal numbers 
df1$danceability <-(gsub(",", ".", df1$danceability))
df1$loudness <-(gsub(",", ".", df1$loudness))
df1$valence <-(gsub(",", ".", df1$valence))
df1$tempo <-(gsub(",", ".", df1$tempo))

#Convert to factor
colfactors <-c('loudness','danceability','valence','tempo')
df1[,colfactors]<-lapply(df1[,colfactors],as.numeric)

df1$loudness <-as.numeric(df1$loudness)
df1$danceability <-as.numeric(df1$danceability)
df1$valence <-as.numeric(df1$valence)
df1$tempo <-as.numeric(df1$tempo)

#Convert to factor 
df1$performer <- as.factor(df1$performer)

long_annotation <-"Longest running songs on Billboard Hot 100:\nNo, No, No - 35 weeks \nHalo - 31 weeks; Stole - 20 weeks"

df1 <-na.omit(df1)

#Annotation is done individually here because using geom_text automatically
#has too much overlap. I picked songs heard on radio often 

ggplot(df1,aes(x = loudness, y = tempo, colour = performer)) +
  geom_point(size = 3)+ scale_colour_manual(values = c("#5470FF","#EF6A00","#00B56D")) +
  annotate("text", x = -15.4, y = 173, label = "Irreplaceable") +
  annotate("text", x = -2, y = 164, label = "Survivor") +
  annotate("text", x = -3.5, y = 142, label = "Say my name") +
  annotate("text", x = -5, y = 197, label = "Single Ladies(Put a ring on it)") +
  annotate("text", x = -8.5, y = 205, label = "Naughty girl") +
  annotate("text", x = -5.9, y = 83, label = "Halo") +
  annotate("text", x = -7.3, y = 67, label = "1+1") +
  annotate("text", x = -3.6, y = 186, label = "No,No,No")+
  annotate("text", x = -9, y = 142, label = "Kisses Down Low")+
  annotate("text", x = -5, y = 82, label = "Stole")+
  annotate("text", x = -6, y = 99, label = "Can't nobody")+
  annotate("text", x = -16, y = 34, label = "<--increasing loudness") +
  annotate("text", x = -1.5, y = 34, label = "less loudness-->") +
  annotate("text", x = -13.8, y = 60, label = long_annotation, fontface = 'italic', fill = "white")+
  coord_cartesian(ylim = c(50, 210), clip = "off") +
  theme_minimal() +
  labs(title = "Loudness vs tempo: Destiny's child, Beyoncé & Kelly Rowland music", x = "dB(decibels)", y = "tempo (beats per minute-BPM)")+
  theme(plot.title = element_text(color = "#6D006E", hjust = 0.5))+
  theme(legend.position = "bottom")+
  theme(legend.title=element_blank())+
  theme(plot.background = element_rect(fill = "#E8E6F6"))+
  theme(panel.background = element_rect(fill = "#FFF4FF", color = "#DBD0DB"))
  
#Save the plot
ggsave("destinymusic.png",width = 297,height = 210,units = c("mm"),dpi = 300)


  
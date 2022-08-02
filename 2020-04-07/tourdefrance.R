library(tidyverse)
library(ggflags)
library(showtext)
library(countrycode)

font_add_google("Lobster","lobster")
showtext_auto()

tuesdata <- tidytuesdayR::tt_load('2020-04-07')

tdf_winners <- tuesdata$tdf_winners
stage_data <- tuesdata$stage_data
tdf_stages <- tuesdata$tdf_stages

#winning countries
df <- tdf_winners %>%
  count(nationality)

df$code <- countrycode(df$nationality,"country.name","iso2c")
df$code <- tolower(df$code)

df$nationality <- as.factor(df$nationality)

df %>%
  mutate(color = case_when(
    row_number() == 14 ~ 'gray53',
    row_number() == 6 ~ 'gold1',
    TRUE ~"lightblue3"
    )
    ) %>%
  mutate(nationality = reorder(nationality, n)) %>%
  ggplot(aes(x = n, y = nationality,fill = color))+
  geom_col( )+
  geom_flag(x = -0.6, aes(country = code), size = 10)+
  xlab(" ")+ ylab(" ")+
  labs(title = "Le Tour: winners by nationality (1903-2019)",
       subtitle = "bars represent total number of wins in the Generale Classification, i.e. Maillot Jaune",
       caption = "#TidyTuesday April 4 2020|Data: tdf package by Alastair Rushworth|viz: Susan Gichuki,@swarau")+
  scale_fill_identity(guide = "none")+
  annotate("text", x= 21, y = 8.8, label = "Tainted wins: Lance Armstrong of the USPS team was stripped of 7 titles\nfrom 1999-2005 for doping",lineheight = 0.3, color = "firebrick1",size = 14) +
  geom_segment(aes(x = 12, y = 9, xend = 10, yend = 11),arrow = arrow(length = unit(2, "mm")))+ #curvature +ve value produces left hand curve;-ve value produces right hand curve
  theme(plot.title = element_text(family = "lobster", size = 60),
        plot.caption = element_text(size = 32),
        plot.subtitle = element_text(size = 36),
        axis.text = element_text(size = 36, face = "bold"),
        plot.background = element_rect(fill = "#FEF6FF"),
        panel.background = element_rect(fill = "#FEF6FF"),
        axis.line.y = element_line(color = "gray88"),
        axis.line.x = element_line(color = "gray88"))+
  scale_x_continuous(position = "top")


#Save the plot as a PNG file  
ggsave(last_plot(),file = "letour_wins.png",width = 297,height = 210,units = c("mm"),dpi = 300)

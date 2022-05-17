library(tidyverse)
library(showtext)
library(MetBrewer)

font_add_google(name = "Bebas Neue", family = "Bebas Neue")
font_add_google(name = "Outfit", family = "Outfit")

showtext_auto()

nyt_titles <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv')
nyt_full <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv')

#top 5 books for each decade
top5_df <- nyt_titles %>%
  mutate(
    decade = (year %/% 10) * 10) %>%
  group_by(decade)%>%
  slice_max(total_weeks, n=5)

ggplot(top5_df, aes(x = year, y = total_weeks,fill = title)) + 
  geom_bar(stat = "identity", width = 0.9, position=position_dodge(width = 0.5))+
  xlab(" ") + ylab("total no. of weeks in bestseller list")+
  labs(title ="Bestselling books per decade",
       subtitle = "the top 5 from the New York Times bestsellers list",
       caption = "#TidyTuesday May 10 2022|Data: Post45 data|Susan Gichuki@swarau")+
  geom_text(data=top5_df, aes(x=year, y=total_weeks, label=title), color="black", fontface="bold",alpha=0.6, size=8,angle = 90,inherit.aes = FALSE,hjust = 1, check_overlap = T)+ #angle= top5_df$angle,
  geom_vline(xintercept = 1937.5,linetype = "solid",color = "grey",lwd = 1.8)+
  geom_vline(xintercept = 1950,linetype = "solid",color = "grey",lwd = 1.8)+
  geom_vline(xintercept = 1975,linetype = "solid",color = "grey", lwd = 1.8)+
  geom_vline(xintercept = 2000,linetype = "solid",color = "grey", lwd = 1.8)+
  geom_hline(yintercept = 0, linetype = "solid", color = "grey",lwd = 2.2)+
  scale_fill_manual(values = met.brewer("Klimt", n = 51))+
  theme(legend.position = "none",
        plot.title = element_text(family = "Bebas Neue", size = 64),
        plot.subtitle = element_text(family = "Outfit", size = 50),
        plot.caption = element_text(family = "Outfit", size = 34),
        axis.text = element_text(family = "Outfit", size = 38),
        axis.title = element_text(family = "Outfit", size = 38),
        panel.background = element_rect(fill = "#A07630"),
        plot.background = element_rect(fill = "#DFE0DF"),
        panel.grid =element_blank())

#Save the plot as a PNG file  
ggsave(last_plot(), file = "nytbestsellers.png",width = 297,height = 210,units = c("mm"),dpi = 300)


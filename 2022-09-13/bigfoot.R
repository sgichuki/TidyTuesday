library(tidyverse)
library(MetBrewer)
library(rnaturalearth)
library(tmap)
library(patchwork)
library(showtext)

font_add_google("Permanent Marker","permanentmarker")

showtext_auto()

#load data 
bigfoot <- read_csv("~/GitHub/TidyTuesday/2022-09-13/bfro_reports_geocoded.csv")


sightings <- bigfoot %>% 
  group_by(factor(state))%>% 
  count()

names(sightings) <- c("state","total")

plot1 = ggplot(sightings)+
  geom_col(aes(x = reorder(state,total), y = total), fill = "#5F77A9")+
  coord_flip()+
  labs(x = "", y = "",subtitle = "Cumulative no. of Bigfoot(Sasquatch) sightings per state (1997-2021)") +
  theme(plot.background = element_rect(fill = "#E6F4F1"),
        panel.background = element_rect(fill = "#E6F4F1"),
        plot.subtitle = element_text(size = 44),
        axis.text.y = element_text(color = "#6768B4", face = "bold"))

# extracting US states and county data

states <- map_data("state")
counties <- map_data("county")
mapwashington <- subset(states, region == "washington")

bigfoot$season <- as.factor(bigfoot$season)

bigfoot <- bigfoot %>%
  filter(season != "Unknown")

df <- bigfoot %>%
  filter(state == "Washington" & longitude > -130)

plot2 = ggplot() + 
  geom_polygon(data = mapwashington, 
               aes(x=long, y = lat), fill = "#FFF5FD", colour = "#515051") +
  #geom_sf(fill = "blue")+ ##FFF5FD
  geom_point(data = df,
             aes(x=longitude,
                 y=latitude), color = "#5F77A9")+
  facet_wrap(~season)+
  labs(x = "", y = "", title = "Across all seasons: Bigfoot sightings in Washington State")+
  theme(plot.background = element_rect(fill = "#E6F4F1"),
        panel.background = element_rect(fill = "#E6F4F1"),
        strip.text = element_text(size = 42,face = "bold"),
        strip.background = element_rect(fill = "#C3A1BD"))

plot1 /plot2 + plot_annotation(title = "SASQUATCH",
                               subtitle = "Bigfoot refers to a large, hairy, humanlike creature believed by some people to exist in the northwestern United States and western Canada.",
                               caption = "#TidyTuesday Sep 13 2022|Data: Bigfoot Field Researchers Organisation(BFRO) by way of Data.World|viz: Susan Gichuki,@swarau") &
  theme(plot.title = element_text(size = 60,family = "permanentmarker", hjust = 0.5, color = "#C44F6C"),
        plot.caption = element_text(size = 36),
        plot.subtitle = element_text(face = "bold"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        plot.background = element_rect("#E6F4F1", color ="#E6F4F1"),
        text = element_text(size = 34))

#Save the map
ggsave(
  filename = "bigfoot_sightings.png",
  device = "png",
  dpi = "retina",
  height = 12,
  width = 10)

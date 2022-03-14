library(tidyverse)
library(janitor)
library(sf)
library(rnaturalearth)
library(showtext)

# Load fonts ----
font_add_google(name = "Calistoga", family = "Calistoga")
font_add_google(name = "Newsreader", family = "Newsreader")
showtext_auto()

#load data 
stations <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv')

# You now have it in your current working directory, have a look!
stations <- janitor::clean_names(stations)

#aggregate electric infrastructure by state
elecagg <- stations %>%
  select(city,fuel_type_code,state,country,latitude,longitude) %>%
  group_by(factor(state)) %>%
  filter(fuel_type_code == "ELEC")%>%
  count()%>%
  arrange(desc(n))

#filter data from california
stations$state <- as.factor(stations$state)

california <- stations %>%
  filter(factor(state) == "CA")


data_df <- data.frame(california$city,
                      california$fuel_type_code,
                      california$state,
                      california$country,
                      california$access_code,
                      california$latitude,
                      california$longitude)


names(data_df) <- c("city",
                    "fuel_type_code",
                    "state",
                    "country",
                    "access_code",
                    "latitude",
                    "longitude")

# load data
world <- ne_countries(returnclass = "sf")

data_df$fuel_type_code <- as.factor(data_df$fuel_type_code)

# extracting US states and county data

states <- map_data("state")
counties <- map_data("county")
mapcali <- subset(states, region == "california")

elec_cali<-data_df %>%
  filter(fuel_type_code == "ELEC" & longitude < -80)

elec_cali$access_code <-as.factor(elec_cali$access_code)

ggplot() + 
  geom_polygon(data = mapcali, 
               aes(x=long, y = lat)) +
  geom_sf(fill = "#FFF5FD")+
  geom_point(data = elec_cali,
             aes(x=longitude,
                 y=latitude,color = access_code))+
  labs(title = "Electric charging stations in California",
       subtitle = "At 14608,California has the most electric charging stations in the US.",
       caption = "#TidyTuesday March 1 2022|Data: US DOT|viz:Susan G@swarau",
       color = "Access type")+
  theme_void()+
  theme(plot.caption = element_text(hjust = 1,family = "Newsreader",size = 32),
        plot.title = element_text(size = 60,hjust = 0,family = "Calistoga",color = "#105DC8",margin = margin(b = 12.5)),
        plot.subtitle = element_text(size = 52,hjust = 0,color = "#105DC8",family = "Newsreader"),
        legend.title = element_text(size = 36, face = "bold",family = "Newsreader"),
        legend.text = element_text(family = "Newsreader", size = 32),
        legend.position = "bottom",
        plot.background = element_rect(fill = "#EEE8A9",color = "#E2DED0"),
        text = element_text(family = "Newsreader"))+
  scale_fill_manual(values = c("#B92D00","#24C196"),name = "Access type")

#Save the map
ggsave(
  filename = "electricstations_cali.png",
  device = "png",
  dpi = "retina",
  height = 7,
  width = 8)







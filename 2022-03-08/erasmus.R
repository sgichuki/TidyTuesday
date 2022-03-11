library(tidyverse)
library(countrycode)
library(showtext)
library(stringr)
library(tidygeocoder)
library(maps)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Load fonts ----
font_add_google(name = "Galada", family = "Galada")
font_add_google(name = "Arimo", family = "Arimo")
showtext_auto()


#load data 
erasmus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv')

country_codes <- countrycode::codelist %>%
  select(iso2c, country_name = country.name.en)

#filter data for most recent year 
data_df <- erasmus %>%
  filter(factor(academic_year) == "2019-2020")

#filter the columns you need
data2 <-data.frame(data_df$sending_country_code,
                   data_df$sending_city,
                   data_df$receiving_country_code,
                   data_df$receiving_city,
                   data_df$mobility_duration,
                   data_df$participants)

#rename the columns in the new dataframe
names(data2) <- c("sending_country_code",
                  "sending_city",
                  "receiving_country_code",
                  "receiving_city",
                  "mobility_duration",
                  "participants")

#filter out records where home city is the same as the destination city
data2 <-data2%>%
  filter(sending_country_code!=receiving_country_code)

#filter out students from France 
data2 <-data2 %>%
  filter(sending_country_code == "FR")

#change the names of cities to Title Case -using function from stringr
data2$sending_city <-str_to_title(data2$sending_city)
data2$receiving_city <- str_to_title(data2$receiving_city)

# remove rows in r by row number - city is missing in this row
data2 <- data2[-c(77),] 

#process to get city GPS coordinates
homecity <-unique(data2$sending_city)
hostcity <-unique(data2$receiving_city)

cities <- data.frame(c(homecity, hostcity))
names(cities) <-c("cities")

#use tidygeocoder to get the GPS coordinates
coordinates <- geo(address = cities$cities)

#define the network links i.e. edges
edges <-
  data2 %>%
  inner_join(coordinates, by = c("sending_city" = "address")) %>%
  rename(x = long, y = lat) %>%
  inner_join(coordinates, by = c("receiving_city" = "address")) %>%
  rename(xend = long, yend = lat)

edges %>% 
  add_count(sending_city) %>%
  add_count(receiving_city)

node_size <-
  edges %>%
  select(sending_city,receiving_city) %>%
  pivot_longer(everything(), values_to = "city") %>%
  count(city)

names(coordinates) <-c("city","lat","long")
nodes <-
  coordinates %>%
  inner_join(node_size)

# load data
world <- ne_countries( returnclass = "sf")

#get the map labels
maplabels <-data.frame(unique(edges))


ggplot(data = world) +
  geom_sf(fill = "#FFF5FD") +
  labs( x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(-10, 50), ylim = c(30,65), expand = TRUE)+
  geom_point(data = nodes, aes(x = long, y = lat),
           size = log(nodes$n)/2,
           color = "#4F4350") +
  geom_text(data = maplabels, 
            aes(x = xend,y = yend,label = receiving_city),
            size = 14,check_overlap = TRUE)+
  geom_curve(data = edges, 
             aes(x = x , y = y, xend = xend, yend = yend,     
                 alpha = participants),#
             curvature = 0.33,
             size = 1.5,
             color = "red2")+ #%>% filter (participants >=10)) +
  guides(alpha = guide_legend(title = "students")) +
  theme_void() +
  labs(title = "Erasmus student mobility: Cities hosting French students(2019-2020)",
       caption = "#TidyTuesday-March 8 2022|Data:Data.Europa|viz:Susan G.@swarau") +
  theme(
    plot.margin = unit(c(0,.5,0,.5),"cm"),
    plot.caption = element_text(hjust = 1,family = "Arimo",size = 32),
    plot.title = element_text(size = 60,hjust = 0.5,color = "#105DC8",margin = margin(b = 12.5)),
    legend.title = element_text(size = 36, face = "bold",family = "Arimo"),
    legend.text = element_text(family = "Arimo", size = 32),
    plot.background = element_rect(fill = "#E0E5F6", color = "#E2DED0"),
    text = element_text(family = "Galada")
  ) 

#Save the map
ggsave(
  filename = "erasmus_exchanges.png",
  device = "png",
  dpi = "retina",
  height = 7,
  width = 8)
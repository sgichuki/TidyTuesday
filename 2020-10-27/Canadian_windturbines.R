library(tidyverse)
library(sf)
library(tmap)
library(osmdata)
library(leaflet)
library(rgeos)

#For interactive map viewing using tmap package
tmap_mode("view")

#Load data
wind_turbine<-read.csv("~/GitHub/TidyTuesday/2020-10-27/windturbine.csv")

#wind_turbine[,4,11] <- lapply(wind_turbine[,4,11],as.factor)
#wind_turbine$commissioning_date <- as.Date(as.character(wind_turbine$commissioning_date), format = "%Y")
wind_turbine_df<- wind_turbine[!is.na(wind_turbine$latitude), ]
wind_turbine_df<- wind_turbine[!is.na(wind_turbine$longitude), ]

turbinesperprovince <-wind_turbine %>%
  group_by(factor(province_territory))%>%
  count()
names(turbinesperprovince)<-c("province_territory","total_turbines")

#Total wind turbines per province
totalturbineplot=ggplot(turbinesperprovince,aes(x=reorder(province_territory,total_turbines),total_turbines))+
  geom_bar(stat="identity",fill="dodgerblue")+
  xlab("")+ylab("sum of turbines")+
  geom_text(data=turbinesperprovince,aes(label=total_turbines),hjust=-.1)+
  ggtitle("Total wind turbines per province")+
  coord_flip()+theme_classic()

#Save the plot
ggsave("totalturbineplot.png",width = 297,height = 210,units = c("mm"),dpi = 300)

#How is turbine capacity distributed across these projects?
wind_turbine %>%
  filter(!is.na(turbine_rated_capacity_k_w)) %>%
  ggplot((aes(x = turbine_rated_capacity_k_w))) +
  geom_histogram(
    bins = 7,
    fill = "dodgerblue",
    col = "white",
    alpha = .9)+
  theme_classic()+
  labs(title = "Histogram Of Rated Turbine Capacity", x = "Rated Capacity (kW)", y =
         "Count")

ggsave("turbinecapacityhist.png",width = 297,height = 210,units = c("mm"),dpi = 300)

#Rotor diameter vs hub height
ggplot(wind_turbine,aes(x=rotor_diameter_m, y=hub_height_m))+
geom_point(aes(color=province_territory),size=5,alpha=0.9,shape=16)+
  theme(legend.position = "bottom")+labs(color="province")+
  scale_color_brewer(palette = "Paired")+
  xlab("rotor diameter(m)")+ylab("hub height(m)")+
  ggtitle("Rotor diameter vs hub height")
  

#Map all Canadian wind turbines
windturbine_sf<-
  st_as_sf(wind_turbine, coords = c("longitude","latitude"), 
          crs = "+proj=longlat +datum=WGS84 +no_defs") %>% 
  st_transform(crs = "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45")

sf_canada <-
  rnaturalearth::ne_countries(scale = 110, country = "Canada", 
                              returnclass = "sf") %>% 
  st_transform(crs = st_crs(windturbine_sf))

canadaturbinemap=ggplot(sf_canada) +
  geom_sf(color = NA, fill = "grey92") +
  geom_sf(data = windturbine_sf, color = "darkmagenta", alpha = .1, 
          size = 2, shape = 21, fill = NA)+theme_bw()

ggsave("canadaturbinemap.png",width = 297,height = 210,units = c("mm"),dpi = 300)

tm_basemap("OpenStreetMap")+
  tm_shape(windturbine_sf,bbox="Canada")+tm_dots(popup.vars=c("Project Name"="project_name","Total project capacity(MW)"="total_project_capacity_mw"))

#Map Quebec turbines
quebecturbines<-filter(wind_turbine,province_territory == "Quebec")

quebecturbine_sf<-
  st_as_sf(quebecturbines, coords = c("longitude","latitude"), 
           crs = "+proj=longlat +datum=WGS84 +no_defs") %>% 
  st_transform(crs = "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45")

tm_basemap("OpenStreetMap")+
  tm_shape(quebecturbine_sf,bbox="Canada")+tm_dots("total_project_capacity_mw",n=6,popup.vars=c("Project Name"="project_name","Total project capacity(MW)"="total_project_capacity_mw"),palette="PuRd")




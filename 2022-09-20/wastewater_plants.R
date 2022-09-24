library(tidyverse)
library(tmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(patchwork)
library(showtext)

font_add_google("Rubik","rubik")

showtext_auto()

HydroWASTE_v10 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-20/HydroWASTE_v10.csv')

hydrowaste <- HydroWASTE_v10  
  
hydrowaste <- janitor::clean_names(hydrowaste)

hydrowaste %>%
  filter(coast_10km == 1)%>% 
  group_by(factor(country)) %>%
  count(country,sort = TRUE)%>%
  arrange(desc(n))%>% 
  head(25)%>%
ggplot() +
  geom_col(aes(x = n, y = reorder(country,n)))+
  geom_text(aes(x = n, y= country, label = n))

df1 <- hydrowaste %>%
  filter(country == "France" & lat_wwtp > 40)

#Check how many WWTPs have dilution factor with NAs. 
sum(is.na(df1$df))


plot1 = ggplot() + 
  geom_polygon(data = france, aes(x=long, y = lat, group = group),fill = "gray 99") + 
  coord_fixed(1.3)+
  geom_point(aes(lon_wwtp, lat_wwtp, color = env_concern),
             data = df1 %>%
             mutate(env_concern = if_else(df < 10, "Yes", "No")),
             size = 0.7)+
  scale_color_manual(values = c("royalblue","tomato"))+
  labs(x = "", y = "")+ 
  guides(col=guide_legend("Environmental risk?"))+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 34),
        legend.text = element_text(size = 34),
        legend.background = element_rect(fill ="#FEF7FF"),
        plot.background = element_rect(fill = "#FEF7FF"),
        panel.background = element_rect(fill = "#FEF7FF"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        text = element_text(size = 34),
        axis.text = element_text(face = "bold"))
        

  


df2 <- hydrowaste %>%
  filter(country == "France" & lat_wwtp > 40)%>%
  filter(df < 10)

df2$wwtp_name <-str_to_title(df2$wwtp_name)

plot2 = ggplot(df2,aes(x = waste_dis, y = reorder(wwtp_name,waste_dis))) + 
  geom_col(fill = "#168AA1")+
  labs(x = " Quantité d'eaux usées rejetées en m3/jour", y = "")+
  annotate("text", y = 3.5, x = 80000, label = "Ces stations d'épuration ont des facteurs\n de dilution < 10.Elles présentent un risque\npour l'environnement", lineheight = 0.3, size = 16,color = "#B14E78")+
  theme(axis.text = element_text(face = "bold"),
        plot.background = element_rect(fill = "#FEF7FF"),
        panel.background = element_rect(fill = "#FEF7FF"))

plot1 +plot2 + 
  plot_annotation(title = "Stations d'épuration des eaux usées en France",
                  subtitle = "9 stations d'épuration des eaux usées (STEP) présentent un risque pour l'environnement. Le statut de 645 STEP est inconnu car les facteurs de dilution sont manquants.",
                  caption = "Data:Ehalt Macedo et al: Distribution and characteristics of wastewater treatment plants within the global river network, Earth Syst. Sci. Data, 14, 559–577, https://doi.org/10.5194/essd-14-559-2022, 2022.|#TidyTuesday Sep 20 2022|viz: Susan Gichuki,@swarau") &
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        plot.background = element_rect("#FEF7FF", color ="#FEF7FF"),
        panel.background = element_rect("#FEF7FF", color ="#FEF7FF"),
        plot.title = element_text(size = 60,face = "bold",family = "rubik",hjust = 0.5, color = "#FF0017"),
        plot.subtitle = element_text(size = 40,face = "bold",hjust = 0.5),
        text = element_text(size = 34))
  

#Save the map
ggsave(
  filename = "wwtp_france.png",
  device = "png",
  dpi = "retina",
  height = 8,
  width = 14)



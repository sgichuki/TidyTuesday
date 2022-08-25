library(tidyverse)
library(MetBrewer)
library(patchwork)
library(showtext)

font_add_google("Montserrat","montserrat")
font_add_google("Archivo","archivo")

showtext_auto()

chips <- read_csv('~/GitHub/TidyTuesday/2022-08-23/chip_dataset.csv')

chips <- janitor::clean_names(chips)

#convert character to date
chips$release_date <- as.Date(chips$release_date, format = "%Y-%m-%d")

bgcolor <- "#E8F3F1"

plot1 <- ggplot(chips) +
  geom_point(aes(x = release_date, y = transistors_million,color = factor(foundry)),size = 2,alpha = 0.55)+
              geom_smooth(aes(x = release_date,y= transistors_million, method = "gam"))+
  facet_wrap(~factor(type))+
  scale_y_log10()+
  scale_color_manual(values=met.brewer("Austria",10))+
  labs(title = "Moore's Law",
       subtitle = "the number of transistors on a chip doubles roughly every 18 months while the\nrelative cost decreases")+
  xlab("Release date")+
  ylab("Transistor count in millions,log scale")+
  theme(plot.title = element_text(family = "alice", size = 36, hjust = 0.5),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        #panel.border = element_rect(color = "grey"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14,face = "bold"),
        strip.text.x = element_text(size = 24),
        strip.background = element_rect(fill = "#95B1AF"),
        panel.background = element_rect(fill = "#E8F3F1"),
        legend.background = element_rect(fill = bgcolor),
        plot.margin = margin(20,30,10,30),
        plot.background = element_rect(fill = bgcolor, color = bgcolor)) +
  guides(color = guide_legend(title = "Foundry"))


chips$type <-as.factor(chips$type)

plot2 <- chips %>%
  filter(type == "GPU")%>%
  filter(vendor == "NVIDIA"|vendor == "AMD") %>%
  ggplot()+
  geom_point(aes(x = release_date , y = die_size_mm_2, color = factor(vendor)),size = 2.5,alpha = 0.7)+
  xlab("Release date") + ylab("Die size(mm^2)")+
  scale_color_manual(values = met.brewer("Egypt",2))+
  labs(subtitle = "The largest GPU die sizes are increasing over time.",
       caption = "#TidyTuesday Aug 23 2022|Data:https://chip-dataset.vercel.app/|data viz: Susan Gichuki,@swarau")+
  theme(plot.caption.position = "plot",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.caption = element_text(size = 10),
        plot.subtitle = element_text(size = 14, face = "bold"),
        panel.background = element_rect(fill ="#E8F3F1" ),
        plot.background = element_rect(fill = bgcolor, color = bgcolor),
        legend.position = "right",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14,face = "bold"),
        plot.margin = margin(20,30,10,30),
        legend.background = element_rect(fill = bgcolor))+
  guides(color = guide_legend(title = "Vendor"))

plot1 /plot2 + 
  plot_annotation(theme = 
                    theme(plot.background = element_rect(fill = bgcolor, color = bgcolor)))
  
showtext_opts(dpi = 300) 

ggsave("semiconductor_chips.png", height = 10, width = 8, dpi=300)  

showtext_auto(FALSE)


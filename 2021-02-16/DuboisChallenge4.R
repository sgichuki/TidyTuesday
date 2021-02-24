library(tidyverse)
library(scales)

freed_slaves <- read.csv("~/GitHub/TidyTuesday/2021-02-16/freed_slaves.csv")

#This code maps the percentage labels to the Free column of the data set
# but without the % sign, for that I used the snippet of code below it where 
# the percent labels are matched with the year still using geom_text
ggplot(freed_slaves,aes(x = Year, y = Slave)) +
  geom_area(fill="gray26") +
  scale_x_continuous(expand = c(0,0), 
                     position = 'top',
                     breaks = freed_slaves$Year)+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,100))+
  theme(panel.background = element_rect(fill = "forestgreen",
                                        colour = "black",size = 0.5, 
                                        linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "seagreen"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "forestgreen"))+
  coord_cartesian(clip = "off") +
  ggtitle("PROPORTION OF FREEMEN AND SLAVES AMONG AMERICAN NEGROES.\n\nPROPORTION DES NEGRES LIBRES ET DES ESCLAVES EN AMERIQUE.\n\n DONE BY ATLANTA UNIVERSITY.")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(data=freed_slaves,aes(x = Year, y = Slave, label = Free),
            position = position_nudge(y = 2, x = 1.5), color = 'gray26')


plotfreedslaves=ggplot(freed_slaves,aes(x = Year, y = Slave)) +
  geom_area(fill="gray26") +
  scale_x_continuous(expand = c(0,0), 
                     position = 'top',
                     breaks = freed_slaves$Year)+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,100))+
  theme(panel.background = element_rect(fill = "forestgreen",
                                        colour = "black",size = 0.5, 
                                        linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "seagreen"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "forestgreen"))+
  coord_cartesian(clip = "off") +
  ggtitle("PROPORTION OF FREEMEN AND SLAVES AMONG AMERICAN NEGROES.\n\nPROPORTION DES NEGRES LIBRES ET DES ESCLAVES EN AMERIQUE.\n\n DONE BY ATLANTA UNIVERSITY.")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(1830, 65), color = "#daccbd", label = "SLAVES \nESCLAVES")+
  geom_text(aes(1791, 93), label = "8%",size=4) +
  geom_text(aes(1801, 90), label = "11%",size=4) +
  geom_text(aes(1811, 88), label = "13.5%",size=4) +
  geom_text(aes(1821, 88), label = "13%",size=4) +
  geom_text(aes(1831, 87.5), label = "14%",size=4) +
  geom_text(aes(1841, 88), label = "13%",size=4) +
  geom_text(aes(1851, 89.5), label = "12%",size=4) +
  geom_text(aes(1861, 90), label = "11%",size=4) +
  geom_text(aes(1868, 90), label = "100%",size=4)
  
ggsave("plotfreedslaves.png",width = 297,height = 210,units = c("mm"),dpi = 300)


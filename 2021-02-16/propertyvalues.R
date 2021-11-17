library(tidyverse)
library(janitor)
library(scales)
library(showtext)

#Add fonts 
font_add_google("Lato")
font_add_google("Open Sans Condensed")
showtext_auto()

propertyvalues <- read.csv("~/GitHub/TidyTuesday/2021-02-16/propertyvalue-georgia-duboisdata.csv")

#clean column names
propertyvalues <- janitor::clean_names(propertyvalues)

label1 = "KU-KLUXISM"
label2 = "POLITICAL"
label2a = "UNREST"
label3 = "RISE OF"
label3a = "THE"
label3b = "NEW INDUSTRIALISM"
label4 = "DISFRANCHISEMENT &"
label4a = "PROSCRIPTIVE LAWS"
label5 = "LYNCHING"
label6 = "FINANCIAL PANIC"


ggplot() + 
  geom_line(data = propertyvalues[1:4,],aes(x = year, y = value_millions),size = 2.0,color = "red") +
  geom_line(data = propertyvalues[4:13,], aes(x = year, y = value_millions),size = 2.0) +
  geom_line(data = propertyvalues[13:14,], aes(x = year, y = value_millions),size = 2.0, color = "red") +
  scale_y_continuous(labels=scales::dollar_format()) +
  labs(title = "VALUATION OF TOWN AND CITY PROPERTY OWNED BY GEORGIA NEGROES", 
       caption = "#BlackinData2021,#BlackTIDES,DuboisData; viz:Susan G. @swarau") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab(" ") + ylab(" ") +
  
  annotate('text', x = 1872, y = 650000,label = label1, angle = 90,size = 16) +
  annotate('text', x = 1876, y = 2400000,label = label2,size = 16) +
  annotate('text', x = 1877, y = 2300000,label = label2a,size = 16) +
  annotate('text', x = 1881, y = 4200000,label = label3,size = 16) +
  annotate('text', x = 1881, y = 4100000,label = label3a,size = 16) +
  annotate('text', x = 1884, y = 4000000,label = label3b,size = 16) +
  annotate('text', x = 1897, y = 2500000,label = label4,size = 16) +
  annotate('text', x = 1897, y = 2400000,label = label4a,size = 16) +
  annotate('text', x = 1893, y = 1750000,label = label5,size = 16) +
  annotate('text', x = 1894, y = 1000000,label = label6,angle = 90,size = 16) +
  scale_x_continuous(breaks = seq(1870, 1900, by = 5)) +
  
  theme(panel.background = element_rect(fill = "antiquewhite1",
                                        colour = "black",size = 0.5, 
                                        linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) +
  
  theme(plot.background = element_rect(fill = "antiquewhite")) +
  theme(axis.text = element_text(color="black",size = 34),
        axis.title.y = element_text(family = "Open Sans Condensed",size = 34),
        title = element_text(family = "Lato", face = "bold", size = 36))
  
ggsave("propertyvalues_duboisdata.png",width = 297,height = 210,units = c("mm"),dpi = 300)

  
  
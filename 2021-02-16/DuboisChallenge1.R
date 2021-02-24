#TidyTuesday Week 8 2021-02-16
library(ggplot2)

#Load data
georgia_pop<-read.csv("~/GitHub/TidyTuesday/georgia_pop.csv")


georgiapop_plot=ggplot(georgia_pop)+ 
  geom_line(aes(y=Colored,x=Year,linetype="COLORED")) +
  geom_line(aes(y=White,x=Year,linetype="WHITE")) + coord_flip() +
  scale_linetype_manual(name = "",breaks = c("COLORED", "WHITE"),
                        values = c("COLORED" = "solid", "WHITE" = "longdash")) +
  theme(legend.position = "bottom") +
  scale_x_reverse()+ylab("Percents") + 
  scale_x_continuous(breaks=seq(1790,1890,10)) +
  theme(panel.background = element_rect(fill = "burlywood1",
                                        colour = "black",size = 0.5, 
                                        linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "orangered"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "orangered")) +
  ggtitle("Comparative increase of white and colored population of Georgia")+
  theme(plot.title = element_text(hjust = 0.5))

ggsave("georgiapop_plot.png",width = 297,height = 210,units = c("mm"),dpi = 300)
  
  
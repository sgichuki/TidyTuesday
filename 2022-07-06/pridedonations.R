library(tidyverse)
library(janitor)
library(RColorBrewer)
library(treemapify)
library(patchwork)

#load data using the TidyTuesday package
tuesdata <- tidytuesdayR::tt_load('2022-06-07')

pride_aggregates <- tuesdata$pride_aggregates
fortune_aggregates <- tuesdata$fortune_aggregates
static_list <- tuesdata$static_list
pride_sponsors <- tuesdata$pride_sponsors
corp_by_politicians <- tuesdata$corp_by_politician
donors <- tuesdata$donors

corp_by_politicians <- janitor::clean_names(corp_by_politicians)

#remove last row which has total 
corp_by_politicians <- corp_by_politicians[-c(103),] 
fortune_aggregates <- fortune_aggregates[-c(117),]


#change column to factor type
corp_by_politicians$state <- as.factor(corp_by_politicians$state)
corp_by_politicians$title <- as.factor(corp_by_politicians$title)

#states with most contributions from corporations
df <- corp_by_politicians %>%
  group_by(state) %>%
  count(politician) %>%
  summarise(sum(n))

names(df) <- c("state","total_politicians")

mylabel = "The Money Trail:
          \nCorporate donations to Tennessee state
          \nrepresentatives known to promote
          \nanti-LGBTQ policies.
          \nAt $2M and 300K respectively,
          \ngovernors Greg Abbott(Texas) and
          \nKay Ivey(Alabama) received the most
          \namount of money from corporations.
          \nHowever, these are outliers,
          \nmost contributions were smaller amounts
          \ngiven by companies to state
          \nrepresentatives."

#State Representatives with highest contributions
plot1 = corp_by_politicians %>% 
  filter(title == "Representative") %>%
  arrange(desc(sum_of_amount)) %>%
  top_n(15) %>%
ggplot()+
  geom_col(aes(x = reorder(politician,sum_of_amount), y = sum_of_amount), fill = "#F02D17")+
  coord_flip()+
  xlab(" ")+ ylab("contributions in $")+
  annotate("text", x = 9, y = 14000,label = mylabel,lineheight = 0.5,hjust = 0)+
  theme(panel.background = element_rect(fill = "#EEFCFD"),
        axis.line.y = element_line(color = "grey"),
        axis.text.y = element_text(face = "bold"))

fortune_aggregates <- janitor::clean_names(fortune_aggregates)

df2 <-
  fortune_aggregates %>%
  arrange(desc(number_of_politicians_contributed_to))%>%
  top_n(9)

df2$total_contributed <- (round(df2$total_contributed))

plot2 = ggplot(df2, aes(area = total_contributed, 
                fill = factor(company), 
                subgroup = number_of_politicians_contributed_to,
                label = paste(company, scales::comma(round(`total_contributed`),sep = "\n",subgroup = company))))+ 
  geom_treemap()+
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15)+
  geom_treemap_subgroup_text(place = "left", grow = TRUE,
                             alpha = 0.25, colour = "black",
                             fontface = "italic")+
  geom_treemap_subgroup_border(colour = "#111111", size = 3)+
  theme(legend.position = "none")

patchwork <- plot2 |plot1 

patchwork + plot_annotation(
  title = 'Corporate donations to anti-LGBTQ politicians',
  subtitle = 'Companies support Pride, a celebration of the LGBTQ community, but data shows they also donate to hundreds of state politicians responsible\nfor policies harmful to the LGBTQ community.',
  caption = '#TidyTuesday June 7 2022|Data:Data for Progress|viz:Susan Gichuki@swarau')

ggsave(last_plot(),file = "pridedonations.png",width = 297,height = 210,units = c("mm"),dpi = 300)
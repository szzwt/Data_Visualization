# import relative packages

library(dplyr)
library(ggplot2)
library(gganimate)
library(gifski)

#load data

data <- read.csv("data/crudeoil_production.csv")

# filter the data that is unknown. the TIME >= 2000

filtered_data <- data[data $ Flag.Codes != "L" & data$TIME >= 2000, ]

# group data by TIME
# arrange the data in order by value
# only remain the top 10

data_top10 <- filtered_data %>%
  group_by(TIME) %>%
  arrange(TIME, desc(Value)) %>%
  mutate(ranking = row_number()) %>%
  filter(ranking <= 10)

# plot the static data of each year 
  ## set the axis, text and label of the plot
  ## filp the axis
  ## theme layout

plot <- data_top10 %>%
  ggplot() +
  geom_col(aes(ranking, Value, fill = LOCATION)) +
  geom_text(aes(ranking, Value,label = as.character(Value)), hjust = -0.1) +
  geom_text(aes(ranking, y=0, label = LOCATION), hjust = 1.1) + 
  geom_text(aes(x = 10, y = max(Value) , label = as.factor(TIME)), vjust = 0.2, alpha = 0.5,  col = "gray", size = 20) +
  scale_y_continuous(label = function(x) paste(round(x / 1000, 2), " k"))+
  coord_flip(clip = "off", expand = FALSE) + scale_x_reverse() +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(), 
    legend.key.size = unit(0.5, "cm"), 
    legend.position = c(1.0, 0.3),
    legend.title=element_blank(), 
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(1, 4, 1, 3, "cm"),
    plot.title = element_text(face = "bold", hjust = 0.5),
  ) +
  guides(fill = guide_legend(ncol = 2))

# animation and save
 ## animate change by TIME
 ## smoothing treatment

plot_transition <- plot+transition_states(TIME, state_length = 0, transition_length = 2) +
  labs(title = 'crudeoil production around the world(Top 10)',  
       caption  = "crudeoil production in KTOE        | Data Source: OECD Data",
      ) +
  enter_fade() +
  exit_fade() + 
  ease_aes('quadratic-in-out') 

animate(plot_transition,renderer=gifski_renderer("gif/crudeoil_production_visualization.gif"))


library(tidyverse)
library(plotly)


t_data <- read_csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>% 
  select(year = Year, all_of(month.abb)) %>% 
  pivot_longer(-year, names_to = "month", values_to = "t_diff") %>%
  drop_na() %>% 
  mutate(month = factor(month, levels = month.abb)) %>% 
  arrange(year, month) %>% 
  mutate(month_number = as.numeric(month),
         radius = t_diff + 1.5,
         theta = 2 * pi * (month_number-1)/12,
         x = radius * sin(theta),
         y = radius * cos(theta),
         z = year)

#t_data %>% 
 #   ggplot(aes(x=x, y=y, color=z)) +
  #  geom_path()


plot_ly(t_data,
         x = ~x, y = ~y, z = ~z,
         type = 'scatter3d',
         mode = 'lines',
         line = list(width = 4, color = ~t_diff,
                     colorscale = list(c(0,'#BA52ED'), c(1, '#FCB040'))))
         
         
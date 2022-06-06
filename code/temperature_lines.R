library(tidyverse)


t_diff <- read_csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>%
  select(year = Year, month.abb) %>% 
  pivot_longer(-year, names_to = "month", values_to = "t_diff") %>% 
  drop_na() %>% 
  mutate(month = factor(month, levels = month.abb))

last_dec <- t_diff %>% 
  filter(month == "Dec") %>% 
  mutate(year = year - 1,
         month = "last_Dec")
next_jan <- t_diff %>% 
  filter(month == "Jan") %>% 
  mutate(year = year + 1,
         month = "next_Jan")

bind_rows(last_dec, t_diff, next_jan) %>%
  mutate(month = factor(month, levels = c("last_Dec", month.abb, "next_Jan")),
         month_number = as.numeric(month) - 1) %>%  
  ggplot(aes(x=month_number, y=t_diff, group=year, color=year)) +
  geom_hline(yintercept = 0, color = "white") +
  geom_line() +
  scale_x_continuous(breaks = 1:12,
                     labels = month.abb,
                     sec.axis = dup_axis(name = NULL, labels = NULL)) +
  scale_y_continuous(breaks = seq(-2, 2, 0.2),
                     sec.axis = dup_axis(name = NULL, labels = NULL)) +
  scale_color_viridis_c() +
  coord_cartesian(xlim = c(1,12)) +
  labs(x = NULL,
       y = "Temperature change sine pre-industrial time [\u00B0C]",
       title = "Global temperature change since 1880 by month") +
  theme(
    panel.background = element_rect(fill = "black", color = "white", size = 1),
    plot.background = element_rect(fill = "#444444"),
    panel.grid = element_blank(),
    axis.text = element_text(color = "white", size = 13),
    axis.ticks = element_line(color = "white", size = 1.5),
    axis.ticks.length = unit(-5, "pt"),
    axis.title = element_text(color = "white", size = 13),
    plot.title = element_text(colour = "red", hjust = 0.5, size = 18)
  )
  
ggsave("figures/temperature_lines.png", width = 8, height = 4.5)

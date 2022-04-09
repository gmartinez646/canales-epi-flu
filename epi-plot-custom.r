epi_plot_channel_custom <- function (joined_channel, year, n_breaks = 10)
{
  joined_channel %>% 
    group_by(var_admx) %>% 
    mutate(new = max(c(var_event_count, upp_95), na.rm = T), ) %>%
    ungroup() %>% ggplot(aes(x = var_week, y = var_event_count)) + geom_area(aes(x = var_week, 
                                                                                                                                                         y = new), fill = "#981000", alpha = 0.6, stat = "identity") + 
    geom_area(aes(x = var_week, y = upp_95), fill = "#fee76a", 
              stat = "identity") + geom_area(aes(x = var_week, 
                                                 y = median), fill = "#3e9e39", stat = "identity") + 
    geom_area(aes(x = var_week, y = low_95), fill = "white", 
              stat = "identity") + geom_line(size = 0.8, aes(color=year)) + scale_color_manual(name = "", values = 1)  + scale_x_continuous(breaks = scales::pretty_breaks(n = {
                {
                  n_breaks
                }
              }))
}

###EJEMPLO

epi_plot_channel_custom(join, "2021")+
  labs(title = "Corredor gripe 2014 - 2020",
       caption = "Fuente: SNVS",
       x = "Semanas",
       y = "Casos",
  ) +
  labs(color = "Name") +
  theme_bw()

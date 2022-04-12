fallecidos_2022 <- read.csv("C:/Users/nicolas.ibarra/Downloads/fallecidos2022 (1).csv", sep=";")
fallecidos_2021 <- read.csv("C:/Users/nicolas.ibarra/Downloads/fallecidos2021.csv", sep=";")

fallecidos_anuales <- dplyr::bind_rows(fallecidos_2021, fallecidos_2022)

fallecidos_anuales$inicio <- lubridate::date(lubridate::parse_date_time(paste(fallecidos_anuales$anio, fallecidos_anuales$SEMEPI, 0, sep="/"),'Y/W/w'))

library(ggplot2)
library(lubridate)

ggplot(fallecidos_anuales, aes(x=inicio, y=fallecidos, group=sexo, colour=sexo )) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels="%V", date_breaks="month", expand=c(0,0)) +
  labs(title = "Mortalidad por Sexo - 2021/2022", x = "Año", y = "Cantidad de Fallecidos") +
  facet_grid(~ year(inicio), space="free_x", scales="free_x", switch="x") +
  theme_bw() +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill=NA,colour="grey50"),
        panel.spacing=unit(0,"cm"))

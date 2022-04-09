install.packages("epichannel")
if(!require("devtools")) install.packages("devtools")
devtools::install_github("avallecam/epichannel")

install.packages("tidyverse")

library(tidyverse)
library(readxl)
library(epichannel)


gripe_excel <- read_excel("C:/Users/nicolas.ibarra/Downloads/flu.xlsx")
View(gripe_excel)

pob <- read_excel("C:/Users/nicolas.ibarra/Downloads/flu.xlsx", 
                  sheet = "Hoja2")

year <- vector()
week <- vector()
week_date <- vector()
num_case <- vector()

for(i in 1:nrow(gripe_excel)) {
  y <- gripe_excel$Año[i]
  
  for (w in 1:(ncol(gripe_excel) - 1)) {
    #print(lubridate::date(lubridate::parse_date_time(paste(y, w, 0, sep="/"),'Y/W/w')))
    
    year <- c(year, y)
    week <- c(week, w)
    week_date <- c(week_date, lubridate::date(lubridate::parse_date_time(paste(y, w, 0, sep="/"),'Y/W/w')))
    num_case <- c(num_case, gripe_excel[[i, w + 1]])
  }
  
}

rm(y)
rm(w)
rm(i)

flu <- data.frame(year, week, week_date, num_case)

as.Date(week_date[2], origin = "1970-01-01")

fluok<-flu%>%
  mutate(adm="flu")

pob<-pob%>%
  mutate(adm="flu")

adapt<-epi_adapt_timeserie(db_disease = fluok,
                    db_population = pob,
                    var_admx = adm, 
                    var_year = year, 
                    var_week = week, 
                    var_event_count = num_case,
                    var_population = poblacion)


disease_now <- adapt %>%
  filter(var_year==max(var_year))

disease_pre <- adapt %>%
  filter(var_year!=max(var_year))

canal <-
  epi_create_channel(time_serie = disease_pre,
                     disease_name = "gripe",
                     method = "gmean_1sd")


join<-epi_join_channel(disease_channel = canal,
                 disease_now = disease_now)

epi_plot_channel(join)+
  labs(title = "Corredor gripe 2014-2020",
       caption = "fuente: SNVS",
       x = "semanas",
       y = "cases",
       ) +
  theme_bw()

epi_plot_channel_custom(join, "2021")+
  labs(title = "Corredor gripe 2014 - 2020",
       caption = "Fuente: SNVS",
       x = "Semanas",
       y = "Casos",
  ) +
  labs(color = "Name") +
  theme_bw()



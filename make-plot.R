library(tidyverse)
library(lubridate)
library(gganimate)
library(gifski)

# Data from: https://datos.covid-19.conacyt.mx/#DownZCSV

cases <- "Casos_Diarios_Municipio_Confirmados_20200627.csv" %>% 
  read_csv() %>% 
  rename(
    id = cve_ent,
    pop = poblacion,
    name = nombre
  ) %>% 
  pivot_longer(
    cols = matches("\\d{2}-\\d{2}-\\d{4}"), 
    names_to = "date", 
    names_transform = list(date = dmy),
    values_to = "cases_new",
    values_transform = list(date = as.integer)
  ) %>% 
  arrange(id, date) %>% 
  group_by(id) %>% 
  mutate(cases_acc = cumsum(cases_new)) %>% 
  ungroup() 


p <- cases %>% 
  # 10 random cities
  filter(id %in% sample(unique(id), 10)) %>% 
  ggplot() +
  aes(x = name, y = cases_acc) +
  coord_flip() +
  geom_col() +
  transition_time(date) +
  ease_aes('cubic-in-out') +
  exit_fade() +
  labs(
    title = 'Cumulative confirmed cases',
    subtitle = '{frame_time}'
  )

animate(p, duration = 5, fps = 20, width = 600, height = 600, renderer = gifski_renderer())

anim_save("covid-cases-plot.gif")

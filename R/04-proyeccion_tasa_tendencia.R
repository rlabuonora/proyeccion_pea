library(tidyverse)
library(forecast)
library(tidyquant)
library(timetk)
library(sweep)
library(lubridate)


theme_set(theme_minimal(base_size = 8) +
          theme(
            plot.title = element_text(hjust=.5)
          ))

df <- readRDS(here::here('data', 'tasas_tramo_edad.rds'))


tasa_uruguay <- df %>% 
  filter(pais == "Uruguay") %>% 
  filter(!str_detect(tramo, "Total")) 
  # Sacar 65+ recientes
  #filter(! (tramo == "Age (5-year bands): 65+" & year(year) > 2004)) %>% 


## Por grupo
nested_ts <- tasa_uruguay %>% 
  group_by(sexo, tramo) %>% 
  nest %>% 
  mutate(
    data_ts = map(
      .x = data,
      .f = tk_ts,
      select=tasa,
      start=1990,
      frequency = 1
    ))

# Random Walk Model: Extrapolar la tasa
#  de crecimiento por tramo de edad
rw_fit <- nested_ts %>% 
  mutate(
    forecast_rw = map(
      .x = data_ts,
      .f = rwf,
      h=70,
      drift=TRUE))

# Tidy
tidy_forecast_rw <- rw_fit %>% 
  mutate(
    sweep = map(forecast_rw, sw_sweep, timetk_idx = TRUE)
  ) %>% 
  unnest(sweep)

# Salvar proyeccion
tidy_forecast_rw %>%
  ungroup %>% 
  select(sexo, tramo, index, tasa, key) %>% 
  saveRDS(here::here('data', 'forecast_tendencia.rds'))



library(tidyverse)
library(forecast)
library(tidyquant)
library(timetk)
library(sweep)
library(lubridate)

# Tasa Fija
# Extrapolar la última tasa de Uruguay (2019) hacia adelante

df <- readRDS(here::here('data', 'tasas_tramo_edad.rds'))


# Solo Uruguay
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

naive_fit <- nested_ts %>% 
  mutate(
    forecast_naive = map(
      .x = data_ts,
      .f = rwf,
      h=70))

forecast_tasas_fijas <- naive_fit %>% 
  mutate(
    sweep = map(forecast_naive, sw_sweep, timetk_idx = TRUE)
  ) %>% 
  unnest(sweep)


forecast_tasas_fijas %>% 
  ungroup %>% 
  select(sexo, tramo, index, tasa, key) %>% 
  saveRDS(here::here('data', 'forecast_tasa_fija.rds'))



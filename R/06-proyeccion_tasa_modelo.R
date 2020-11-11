library(tidyverse)
library(forecast)
library(tidyquant)
library(timetk)
library(sweep)
library(lubridate)

df <- readRDS(here::here('data', 'tasas_tramo_edad.rds'))


tasas_uruguay <- df %>% 
  filter(pais == "Uruguay") %>% 
  select(-pais)
  # Sacar 65+ recientes
  #filter(! (tramo == "Age (5-year bands): 65+" & year(year) > 2004)) %>% 


vacio <- expand_grid(
  sexo = c("Hombres", "Mujeres"),
  year = 2021:2089,
  tramo = c("15-19", "20-24", "25-29", "30-34", 
            "35-39", "40-44", "45-49", "50-54", 
            "55-59", "60-64", "65+")
) %>% 
  mutate(year = as.Date(as.yearmon(year))) %>% 
  mutate(tasa = NA)

# Tasas Modelo
tasas_modelo <- df %>% 
  filter(pais == "Portugal", year(year) == 2020) %>% 
  mutate(year = as.Date(as.yearmon(2090))) %>% 
  select(-pais)


# Interpolar
convergencia_tasas_modelo <- bind_rows(tasas_uruguay, vacio, tasas_modelo) %>% 
  group_by(sexo, tramo) %>% 
  mutate(time=seq(1:n())) %>%
  mutate(ip.value = spline(time, tasa, n=n())$y)


# Salvar proyeccion
forecast_tasas_modelo <- convergencia_tasas_modelo %>%
  mutate(key= if_else(year(year) > 2020, "forecast", "actual"),
         tasa = ip.value) %>%
  select(-ip.value, -time)

saveRDS(forecast_tasas_modelo, here::here('data', 'forecast_tasas_modelo.rds'))

# Salvarlo para ver
# writexl::write_xlsx(forecast_tasas_modelo, 
#                     here::here("output", "forecast_tasas_modelo.xlsx"))


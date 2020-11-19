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
  year = 2021:2099,
  tramo = c("15-19", "20-24", "25-29", "30-34", 
            "35-39", "40-44", "45-49", "50-54", 
            "55-59", "60-64", "65+")
) %>% 
  mutate(year = as.Date(as.yearmon(year))) %>% 
  mutate(tasa = NA)

# Tasas Modelo

# Tasas Uruguay
tasas_uy <- df %>%
  filter(pais == "Uruguay", year(year) == 2019) %>%
  mutate(tasa_uy = tasa, year = as.Date(as.yearmon(2100))) %>%
  select(-pais, -rojos, -tasa)

# Tasas Rojos
tasas_rojos <- df %>%
  filter(rojos, year(year) == 2020) %>%
  group_by(sexo, tramo) %>%
  summarize(tasa_modelo = mean(tasa)) %>%
  mutate(year = as.Date(as.yearmon(2100))) %>%
  ungroup()

# Mezclarlas (tomo las mujeres de los rojos y los hombres de Uruguay)
tasas_modelo <- tasas_rojos %>% 
  inner_join(tasas_uy) %>% 
  mutate(tasa = case_when(
    tramo %in% c("15-19") ~ tasa_uy,
    #tramo %in% c("65+") ~ tasa_modelo,
    sexo == "Mujeres" ~ tasa_modelo,
    TRUE ~ tasa_uy, 
  )) %>% 
  select(sexo, tramo, year, tasa)

# tasas_modelo <- tasas_rojos %>% 
#   rename(tasa = tasa_modelo)
# Tasas modelo solo para las mujeres
#tasas_modelo <- tasas_rojos

writexl::write_xlsx(tasas_modelo, 
                    here::here('output', 'proyecciones', 'tasas_modelo.xlsx'))



# Interpolar
convergencia_tasas_modelo <- bind_rows(tasas_uruguay, vacio, tasas_modelo) %>% 
  group_by(sexo, tramo) %>% 
  mutate(time=seq(1:n())) %>%
  mutate(ip.value = approx(time, tasa, n=n())$y)


# Salvar proyeccion
forecast_tasas_modelo <- convergencia_tasas_modelo %>%
  mutate(key= if_else(year(year) > 2020, "forecast", "actual"),
         tasa = ip.value) %>%
  select(-ip.value, -time)

saveRDS(forecast_tasas_modelo, here::here('data', 'forecast_tasas_modelo.rds'))


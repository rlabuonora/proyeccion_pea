# Importa las planillas con
# las proyecciones de poblacion
# que nos paso BPS

library(tidyquant)
library(readxl)

mujeres <- read_xlsx(
  here::here('data', 'poblacion_ajustada.xlsx'),
  sheet="Mujeres") %>% 
  rename(year=1) %>% 
  select(-MUJERES) %>% 
  mutate(sexo="Mujeres", year=as.Date(as.yearmon(as.numeric(year))))

hombres <- read_xlsx(
  here::here('data', 'poblacion_ajustada.xlsx'),
  sheet="Hombres") %>% 
  rename(year=1) %>% 
  select(-HOMBRES) %>% 
  mutate(sexo="Hombres", year = as.Date(as.yearmon(as.numeric(year))))

pob <- bind_rows(hombres, mujeres) %>% 
  pivot_longer(c(-year, -sexo), names_to="tramo", values_to="pob") %>% 
  #mutate(index = as.Date(as.yearmon(year))) %>% 
  filter(! tramo %in% c("0-4", "5-9", "10-14") ) %>% 
  # Colapsar 65+
  mutate(
    tramo = if_else(
      tramo %in% c( "65-69", "70-74", "75-79", 
                    "80-84", "85-89", "90-94", 
                    "95-99", "100-104", "105-109", 
                    "110-114", "115-119", "120-124", 
                    "125-129", "130 y mas"),
      "65+",
      tramo
    )
  ) %>% 
  group_by(sexo, tramo, year) %>% 
  summarize(pob = sum(pob)) %>% 
  ungroup()
  


saveRDS(pob, here::here('data', 'forecast_poblacion.rds'))

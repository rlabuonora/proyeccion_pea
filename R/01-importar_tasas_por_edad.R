library(tidyverse)
library(lubridate)

# Importar datos OIT
tasas_tramo_edad <- read_csv(here::here('data', 'ilostat-2020-11-04_tramos.csv')) %>% 
  transmute(
    pais = ref_area.label,
    sexo = str_remove(sex.label, "Sex: "),
    #tramo = str_remove(classif1.label, "Age \\((5|10)-year bands\\): "),
    #tramo = str_remove(classif1.label, ".*:\\ "),
    tramo = classif1.label,
    year = as.Date(as.yearmon(time)),
    tasa = obs_value
  ) %>% 
  mutate(sexo = case_when(
    sexo == "Male"~ "Hombres", 
    sexo == "Female"~ "Mujeres",
    TRUE~sexo)) %>% 
  filter(year(year) <= 2020) %>% 
  # Tramos de edad de a 5 años
  filter(str_detect(tramo, "5-year bands")) %>% 
  filter(!str_detect(tramo, "Total")) %>% 
  mutate(tramo = str_remove(tramo, ".*:\\ "))

saveRDS(tasas_tramo_edad, file=here::here('data', 'tasas_tramo_edad.rds'))

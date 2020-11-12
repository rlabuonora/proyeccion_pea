# un df con sexo, tramo, year y tasas
forecast_tasa <- readRDS(here::here('data', 'forecast_tasas_modelo.rds'))

# Salvar las tasas
forecast_tasa %>% 
  mutate(`Año` = year(year)) %>% 
  select(-rojos, -key) %>% 
  pivot_wider(names_from=year, values_from=tasa)

writexl::write_xlsx(forecast_tasa, 
                    here::here('output', 'proyecciones', 'proyeccion_tasas_modelo.xlsx'))


# Visualizar
forecast_tasa %>% 
  ggplot(aes(year, tasa, color = sexo)) +
  geom_line() +
  facet_wrap(~tramo) + 
  labs(title="Tasa de actividad por tramo")

poblacion_forecast <- readRDS(here::here('data', 'forecast_poblacion.rds')) %>% 
  filter(year(year) >= 1990)


# Pegar tablas
df <- poblacion_forecast %>% 
  left_join(forecast_tasa, by=c("year", "sexo", "tramo")) %>% 
  mutate(pea = (tasa * pob) / 100)

# Visualizar proyeccion por edad
df %>% 
  ggplot(aes(year, pea, fill = sexo)) + 
  geom_area() + 
  scale_fill_manual(name = "", values = c("gray70", "#d95f0e")) +
  facet_wrap(~tramo) +
  labs(title="PEA por Tramo de Edad", x="Año", y="", caption="Proyección con tasas modelo")

ggsave(here::here('output', 'proyecciones', 'proyeccion_pea_tasas_modelo.png'))


# Datos agregados al final del periodo
df %>% 
  filter(year(year) == 2100) %>%
  summarize(pea = sum(pea),
            pet = sum(pob)) %>% 
  mutate(tasa_global=pea/pet)

df %>% 
  filter(year(year) == 2100) %>% 
  group_by(sexo) %>% 
  summarize(pea = sum(pea),
            pet = sum(pob),
            tasa = pea/pet)

# Calcular brecha global
df_tasas_globales <- df %>% 
  group_by(sexo, year=year(year)) %>% 
  summarize(pea = sum(pea),
            pob = sum(pob),
            tasa = pea/pob)

brecha <- df_tasas_globales %>% 
  pivot_wider(id_cols=year, names_from=sexo, values_from=tasa) %>% 
  mutate(brecha = Hombres-Mujeres)


brecha_labels <- filter(brecha, year %in% c(2020, 2100))

brecha %>% 
  ggplot(aes(year, brecha)) + 
  geom_line() + 
  geom_point(data=brecha_labels) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  geom_text(data=brecha_labels, 
            nudge_y=3e-2,
            aes(label=scales::percent_format(scale=100, accuracy=.1)(brecha))) + 
  labs(title="Cierre de Brecha Proyectada (2020-2100)", 
       y="",
       x="Año",
       caption="La diferencia entre tasas de actividad de Masculina y Femenina")
  
ggsave(here::here('output', 'proyecciones', 'proyeccion_pea_cierre_brecha.png'))

# Salvar con totales
pea_proyectada <- df %>% 
  select(-rojos) %>% 
  group_by(`Año`=year) %>% 
  summarize(PEA=sum(pea),
            PET=sum(pob))

writexl::write_xlsx(pea_proyectada, here::here('output', 'proyecciones', 'proyeccion_pea_total.xlsx'))


# PEA Total
pea_proyectada %>% 
  mutate(proyeccion = year(`Año`) >= 2020) %>% 
  ggplot(aes(`Año`, PEA)) +
  geom_line(aes(linetype=proyeccion)) +
  labs(title="PEA Total", x="Año") + 
  guides(linetype=FALSE)
  
ggsave(here::here('output', 'proyecciones', 'proyeccion_pea_proyectada.png'))


pea_proyectada_por_sexo <- df %>% 
  select(-rojos) %>% 
  group_by(`Año`=year, sexo) %>% 
  summarize(pea=sum(pea)) %>% 
  pivot_wider(names_from=sexo, values_from=pea, names_prefix="PEA_") %>% 
  mutate(PEA_total = PEA_Hombres+PEA_Mujeres)

writexl::write_xlsx(pea_proyectada_por_sexo, here::here('output', 'proyecciones', 'proyeccion_pea_por_sexo.xlsx'))



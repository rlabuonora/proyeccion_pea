df_countries <- readRDS("~/proyeccion_pea/data/df_countries.rds")

ROJOS <- c("Uruguay", "Czechia", "Greece", "Serbia", "Ukraine", 
           "Montenegro", "Slovakia", "Russian Federation", 
           #"Albania", "Mongolia", 
           "Singapore", 
           #"Cape Verde", 
           "Viet Nam", "Bulgaria", "Estonia", 
           #"Jamaica", 
           "Ireland", "Latvia", "United States", 
           #"Nigeria", 
           "Cyprus", "Austria", "Germany", "Spain", "Switzerland", 
           "Lithuania", "Australia", "Netherlands",
           "Slovenia", "United Kingdom", "Portugal", 
           "Luxembourg", 
           "Canada", "Israel", "Finland", 
           "Norway", "Denmark")

paises <- filter(df_countries, country %in% ROJOS) %>%
  filter(!str_detect(indicador, "Desempleo"))



tasas_global <- paises %>% 
  filter(indicador == "Tasa de actividad, global") %>% 
  ggplot(aes(x=fct_reorder(country, value), value, fill = country == "Uruguay")) + 
  geom_col() + 
  coord_flip() +
  guides(fill=FALSE) +
  facet_wrap(~indicador, scales="free")

tasas_hombres <- paises %>% 
  filter(indicador == "Tasa de actividad, hombres") %>% 
  ggplot(aes(x=fct_reorder(country, value), value, fill = country == "Uruguay")) + 
  geom_col() + 
  coord_flip() +
  guides(fill=FALSE) +
  facet_wrap(~indicador, scales="free")

tasas_mujeres <- paises %>% 
  filter(indicador == "Tasa de actividad, mujeres") %>% 
  ggplot(aes(x=fct_reorder(country, value), value, fill = country == "Uruguay")) + 
  geom_col() + 
  coord_flip() +
  guides(fill=FALSE) +
  facet_wrap(~indicador, scales="free")


pib <- paises %>% 
  filter(indicador == "GDP per cápita") %>% 
  ggplot(aes(x=fct_reorder(country, value), value, fill = country == "Uruguay")) + 
  geom_col() + 
  coord_flip() +
  guides(fill=FALSE) +
  facet_wrap(~indicador, scales="free")


pob_65 <- paises %>% 
  filter(indicador == "Población mayor de 65") %>% 
  ggplot(aes(x=fct_reorder(country, value), value, fill = country == "Uruguay")) + 
  geom_col() + 
  coord_flip() +
  guides(fill=FALSE) +
  facet_wrap(~indicador, scales="free")

(tasas_global + tasas_hombres + tasas_mujeres) / (pib + pob_65)

# Elegir paises

# Tasas UY
tasas_uy <- df_countries %>% 
  filter(country == "Uruguay") %>% 
  mutate(value_uru = value) %>% 
  select(-value, -src_id_ind) %>% 
  filter(indicador %in% c("GDP per cápita", 
                          "Tasa de actividad, mujeres",
                          "Población mayor de 65")) %>% 
  pivot_wider(id_cols = country, 
              names_from=indicador, 
              values_from=value_uru)

pib_uy <- tasas_uy %>% pull(`GDP per cápita`)
tasa_actividad_fem_uy <- tasas_uy %>% pull(`Tasa de actividad, mujeres`)
pob_65_uy <- tasas_uy %>% pull(`Población mayor de 65`)

# Pegar columna
rojos <- df_countries %>% 
  filter(indicador %in% c("GDP per cápita", 
                          "Tasa de actividad, global",
                          "Tasa de actividad, mujeres",
                          "Población mayor de 65")) %>% 
  select(-src_id_ind) %>% 
  pivot_wider(id_cols = country, 
              names_from=indicador, 
              values_from=value) %>% 
  # Esto no cambia nada
  #filter(`GDP per cápita` > pib_uy) %>% 
  filter(`Tasa de actividad, mujeres` > tasa_actividad_fem_uy) %>% 
  filter(`Población mayor de 65` > pob_65_uy)
  



library(tidyverse)


convergencia_tasas_modelo <- readRDS(here::here('data', 'forecast_tasas_modelo.rds'))


# Visualizar las proyecciones con tasas modelo
convergencia_tasas_modelo_df <- convergencia_tasas_modelo %>% 
  mutate(
    y_pos = if_else(sexo=="Mujeres", tasa - 20, tasa + 20)
  )

convergencia_tasas_modelo_df %>% 
  ggplot(aes(year, tasa, color = sexo)) + 
  scale_color_manual(name = "", values = c("gray30", "#d95f0e")) +
  geom_point(show.legend = FALSE, size = 1, 
             data = filter(convergencia_tasas_modelo, 
                           year(year) %in% c(2090))) + 
  geom_text(aes(y = y_pos, 
                label = scales::percent_format(scale=1, accuracy=.1)(tasa)),
            size = 2, 
            nudge_x = -1000,
            show.legend = FALSE, 
            data = filter(convergencia_tasas_modelo_df, year(year) %in% c(2090))) + 
  geom_line(aes(y=tasa, linetype=key)) + 
  facet_wrap(~tramo) +
  guides(linetype=FALSE) +
  labs(title="Proyección con convergencia a tasas modelo",
       x="Año", y = "Tasa", caption="Fuente: OIT")


ggsave(here::here('output', 'forecast_tasas_modelo.png'), width = 7)


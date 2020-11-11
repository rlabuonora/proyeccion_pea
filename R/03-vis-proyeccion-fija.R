library(tidyverse)
library(lubridate)


forecast_tasas_fijas <- readRDS(here::here('data', 'forecast_tasa_fija.rds'))

forecast_tasas_fijas %>% 
  ggplot(aes(index, tasa, color = sexo, linetype=key)) + 
  scale_color_manual(name = "", values = c("gray30", "#d95f0e")) +
  geom_point(show.legend = FALSE, size = 1, 
             data = filter(forecast_tasas_fijas, year(index) %in% c(1990))) + 
  geom_line() +
  facet_wrap(~tramo) +
  labs(title="Proyección con tasas fijas") +
  guides(linetype = FALSE) +
  labs(x="Año", y = "Tasa", caption="Fuente: OIT")

ggsave(here::here('output', 'tasas_fijas.png'), width = 7)



tidy_forecast_rw <- readRDS(here::here('data', 'forecast_tendencia.rds'))


df_tendencial <- tidy_forecast_rw %>% 
  mutate(
    y_pos = if_else(sexo=="Mujeres", tasa - 20, tasa + 20)
  )

df_tendencial %>% 
  ggplot(aes(index, tasa, color = sexo, linetype=key)) + 
  geom_text(aes(y = y_pos, 
                label = scales::percent_format(scale=1, accuracy=.1)(tasa)),
            size = 2, 
            nudge_x = 4000,
            show.legend = FALSE, 
            data = filter(df_tendencial, year(index) %in% c(1990))) + 
  geom_line() +
  geom_point(show.legend = FALSE, size = 1, data = filter(df_tendencial, year(index) %in% c(1990))) + 
  scale_color_manual(name = "", values = c("gray30", "#d95f0e")) +
  facet_wrap(~tramo) +
  labs(title="Proyección con tasas 1990-2020") + 
  guides(linetype = FALSE) +
  labs(x="Año", y = "Tasa", caption="Fuente: OIT")

ggsave(here::here('output', 'tasas_tendencial.png'), width = 7)




library(tidyverse)
library(plotly)
library(viridis)
library(openxlsx)

glimpse(DF3)
DF3 <- DF3 %>% as_tibble()

# Generando gráficos ----

## Marca que más compra (lealtad)

tabla_lealtad <- DF3 %>% 
  # contar los casos
  count(`¿Cuál es la marca que más compra?`) %>% 
  # ordenar la tabla
  arrange(desc(n)) %>% 
  # quitar NINGUNO
  filter(`¿Cuál es la marca que más compra?` != "Ninguno") %>% 
  # calcular los %
  mutate(proporción = n/sum(n),
         porcentaje = scales::percent(proporción))


## Exportado en formato Excel
write.xlsx(x = tabla_lealtad,
           file = "lealtad.xlsx")



## Gráfico de columnas

tabla_lealtad %>% 
  rename(Marca = `¿Cuál es la marca que más compra?`) %>%
  mutate(Marca = factor(Marca),
         Marca = fct_reorder(Marca, n, .desc = T)) %>% 
  
  ggplot(mapping = aes(x = Marca,
                       y = proporción,
                       fill = Marca,
                       label = porcentaje)) +
  geom_col() +
  geom_label(fill = "white") +
  theme_minimal() +
  labs(title = "Lealtad de marca",
       subtitle = "¿Cuál es la marca que más compra?",
       caption = "Conclusión: Johnnie Walker es la marca con mayor índice de lealtad") +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "none",
        # plot.title = element_text(hjust = 0.5),
        # plot.subtitle = element_text(hjust = 0.5),
        # panel.grid.minor.y = element_blank(),
        # panel.grid.major.y = element_blank(),
        # axis.text.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank())










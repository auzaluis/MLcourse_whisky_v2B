
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
  # calcular los %
  mutate(proporción = n/sum(n),
         porcentaje = scales::percent(proporción))


## Exportado en formato Excel
write.xlsx(x = tabla_lealtad,
           file = "lealtad.xlsx")






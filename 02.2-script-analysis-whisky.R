
library(tidyverse)
library(plotly)
library(viridis)
library(openxlsx)

glimpse(DF3)
DF3 <- DF3 %>% as_tibble()

# Generando gráficos ----

## Marca que más compra (lealtad)

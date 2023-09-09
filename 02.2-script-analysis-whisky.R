
library(tidyverse)
library(plotly)
library(viridis)
library(openxlsx)

glimpse(DF3)
DF3 <- DF3 %>%
  as_tibble() %>%
  mutate(across(.cols = everything(),
                .fns = ~ gsub("Johnny Walker", "Johnnie Walker", .)))

# Generando gráficos ----

## Marca que más compra (lealtad)

tabla_lealtad <- DF3 %>% 
  # contar los casos
  count(`¿Cuál es la marca que más compra?`) %>% 
  # ordenar la tabla
  arrange(desc(n)) %>% 
  # eliminar NINGUNO
  filter(`¿Cuál es la marca que más compra?` != "Ninguno") %>% 
  # calcular los %
  mutate(proporción = n/sum(n),
         porcentaje = scales::percent(proporción))


## Exportado en formato Excel
write.xlsx(x = tabla_lealtad,
           file = "lealtad.xlsx")



## Grafico de columnas

tabla_lealtad %>%
  
  # renombrando una columna
  rename(Marca = `¿Cuál es la marca que más compra?`) %>%
  
  mutate(Marca = factor(Marca),
         Marca = fct_reorder(Marca, n, .desc = T)) %>%
  
  ggplot(mapping = aes(x = Marca,
                       y = proporción,
                       fill =Marca,
                       label = porcentaje)) +
  geom_col() +
  
  geom_label(fill = "white") +
  
  theme_minimal() +
  
  labs(title = "Lealtad de marca",
       subtitle = "Cuál es la marca que más compra?",
       caption = "Johnnie Walker es la marca con mayor índice de lealtad") +
  
  scale_fill_viridis_d() +
  
  scale_y_continuous(labels = scales::percent) +
  
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title = element_blank())



## Prueba

DF3 %>% 
  
  pivot_longer(cols = starts_with("Prueba"),
               names_to = "Variable",
               values_to = "Marca") %>%
  select(Marca) %>%
  na.omit() %>%
  count(Marca) %>% 
  mutate(proporción = n/nrow(DF3),
         porcentaje = scales::percent(proporción)) %>% 
  # Ordena el gráfico
  mutate(Marca = factor(Marca),
         Marca = fct_reorder(Marca, n, .desc = T)) %>% 
  
  ggplot(mapping = aes(x = Marca,
                       y = proporción,
                       fill =Marca,
                       label = porcentaje)) +
  geom_col() +
  
  geom_label(fill = "white") +
  
  theme_minimal() +
  
  labs(title = "Prueba de marca",
       subtitle = "Cuáles de estas marcas ha comprado alguna vez?",
       caption = "Johnnie Walker es la marca con mayor % de prueba") +
  
  scale_fill_viridis_d() +
  
  scale_y_continuous(labels = scales::percent) +
  
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title = element_blank())
  
  

## Creación de funciones

table_single <- function(dataFrame, indicador) {
  
  dataFrame %>% 
    count(.data[[indicador]]) %>%
    mutate(Proporción = n/sum(n),
           KPI = rep(indicador, times = nrow(.))) %>% 
    relocate(KPI, .before = indicador)
  
}


tabla_lealtad <-
  table_single(dataFrame = DF3 %>% 
                 rename(Lealtad = `¿Cuál es la marca que más compra?`),
               indicador = "Lealtad") %>% 
  rename(Marcas = Lealtad)



table_multiple <- function(dataFrame, indicador) {
  
  dataFrame %>% 
    pivot_longer(cols = starts_with(indicador),
                 names_to = "Variables",
                 values_to = "Marcas") %>% 
    select(Marcas) %>% 
    na.omit() %>% 
    count(Marcas) %>% 
    mutate(Proporción = n/nrow(dataFrame),
           KPI = rep(indicador, times = nrow(.))) %>% 
    relocate(KPI, .before = Marcas)
  
}

tabla_conocimiento <- table_multiple(dataFrame = DF3,
                                     indicador = "Conocimiento")

tabla_prueba <- table_multiple(dataFrame = DF3,
                               indicador = "Prueba")


bind_rows(tabla_conocimiento,
          tabla_prueba,
          tabla_lealtad) %>%
  
  filter(Marcas != "Ninguno") %>% 
  
  mutate(Porcentaje = scales::percent(Proporción)) %>% 
  
  ggplot(mapping = aes(x = KPI,
                       y = Proporción,
                       fill = KPI,
                       label = Porcentaje)) +
  
  geom_col() +
  
  geom_label(fill = "white", size = 3) +
  
  facet_wrap(~ Marcas) +
  
  theme_minimal() +
  
  scale_fill_viridis_d() +
  
  theme(legend.position = "none",
        axis.title = element_blank())


















library(gsheet)
library(tidyverse)
library(viridis)
library(plotly)



# Importamos la base de datos

DF <- read.csv(text = gsheet2text("https://docs.google.com/spreadsheets/d/13j1EXMDSILZ2IQ0_Hm1IzU974yqOls9_JYf1jvu0nLI/edit#gid=1057441742",
                                  format = "csv"),
               check.names = F,
               stringsAsFactors = F)

# Eliminamos lo que no vamos a necesitar

DF2 <- DF %>% na.omit() %>% select(-c(2:4))



# Cambio de nombre de variables

colnames(DF2)[3:6] <- c("Consideración Chivas",
                        "Consideración Johnny Walker",
                        "Consideración Old Parr",
                        "Consideración Jack Daniel's")

colnames(DF2)[9:12] <- c("Recomendación Chivas",
                         "Recomendación Johnny Walker",
                         "Recomendación Old Parr",
                         "Recomendación Jack Daniel's")


atributos <- colnames(DF2 %>%
                        select(starts_with("Cuáles de estas marcas de whisky")))

atributos

atributos <- c("Precio razonable",
               "Nivel de alcohol óptimo",
               "Que no le haga dar resaca",
               "Presentación del empaque / botella",
               "Ideal para celebrar algo",
               "Que lo haga verse bien",
               "Variedad de maduración (años)")

atributos

colnames(DF2)[14:20] <- atributos



# Separación de respuestas múltiples

DF3 <- DF2 %>%
  
  separate(col = `¿Cuál de estas marcas conoce?`,
           into = c("Conocimiento1",
                    "Conocimiento2",
                    "Conocimiento3",
                    "Conocimiento4"),
           sep = ", ") %>%
  
  separate(col = `¿Cuáles de estas ha comprado alguna vez?`,
           into = c("Prueba1",
                    "Prueba2",
                    "Prueba3",
                    "Prueba4"),
           sep = ", ") %>%
  
  separate(col = `¿Cuál de estos factores son los 2 o 3 más importantes a la hora de elegir una marca de whisky?`,
           into = c("Atributo1",
                    "Atributo2",
                    "Atributo3",
                    "Atributo4",
                    "Atributo5",
                    "Atributo6",
                    "Atributo7"),
           sep = ", ")

library(tidyverse)
library(readr)

df_anotaciones <- readr::read_delim("data/anotaciones/raw/anotaciones_sipecam_20201016.txt", delim = ",")
df_fotos <- readr::read_delim("data/anotaciones/raw/fotos_sipecam_20201016.txt", delim = ",")

selected_columns <- c("cumulo",
                      "nomenclatura_nodo",
                      "integridad_nodo",
                      "ecosistema", 
                      "municipio",
                      "estado",
                      "coord_longitud",
                      "coord_latitud",
                      "fecha_foto",
                      "numero_individuos",
                      "etiqueta",
                      "anotador_id",
                      "ruta" )

df <- df_anotaciones %>% 
  left_join(df_fotos, by=c('ftrampa_id'='id')) %>% 
  select(selected_columns)

write.csv(df, 
          'data/anotaciones/processed/anotaciones_sipecam_20241016.csv', 
          row.names = F)
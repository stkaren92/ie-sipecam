library(tidyverse)
library(broom)
library(terra)

get_test_table <- function(df_counts){

  df_ei <- df_counts %>% 
    group_by(etiqueta) %>% 
    mutate(n_samples=n()) %>% 
    ungroup()
  
  df_ei <- df_ei %>% 
    group_by(etiqueta,Degradado,Integro) %>% 
    mutate(n_duplicate = n()) %>% 
    ungroup() 
  
  df_ei <- df_ei %>% 
    group_by(etiqueta) %>% 
    mutate(estimate = mean(Degradado)-mean(Integro)) %>% 
    ungroup()
  
  df_test1 <- df_ei %>% 
    filter(!(n_samples==n_duplicate)) %>% 
    group_by(etiqueta) %>% 
    do(tidy(t.test(.$Degradado, 
                   .$Integro, 
                   mu = 0, 
                   alt = "two.sided", 
                   paired = TRUE, 
                   conf.level = 0.95))) %>% 
    mutate(n=parameter+1)
  
  df_test2 <- df_ei %>% 
    filter((n_samples==n_duplicate)) %>% 
    group_by(etiqueta, estimate) %>% 
    summarise(n=n())
  
  df_test <- rbind(df_test1,df_test2)
  return(df_test)
}

# Mapa de IE
r_ei <- terra::rast('data/ie/ie_xgb_slic_2021_march_sv.tif')
r_ei <- terra::project(r_ei, "epsg:4326", method='near')

# Número de individuos de cierta especie en una foto
df <- read.csv('data/anotaciones/processed/anotaciones_sipecam_20241016.csv')
df <- df %>% 
  mutate_at(vars(fecha_foto), as.Date, format="%Y-%m-%d")

# Extraer el valor de IE por coordenadas
df <- df %>%
  group_by(coord_longitud, coord_latitud) %>%
  mutate(coord_id = cur_group_id())

df_coord <- df %>% select(coord_longitud,
                          coord_latitud)
df$ie_value <- terra::extract(r_ei, 
                              df_coord,
                        method='exact')$prediction
df$ie <- ifelse(df$ie_value >= 4, 'Degradado', 'Integro')

df$ie_bn_value <- terra::extract(r_ei_bn, 
                              df %>% select(coord_longitud,
                                            coord_latitud),
                              method='exact')$ie2018_250mgw
df$ie_bn <- ifelse(df$ie_bn_value >= 0.75, 'Integro', 'Degradado')

df_aux <- df %>%
  select(coord_latitud,
         coord_longitud,
         ie_value,
         ie,
         ie_bn_value,
         ie_bn,
         integridad_nodo
  ) %>% 
  distinct()
names(df_aux) <- c("coord_latitud",
                   "coord_longitud",
                   "ie",
                   "ie_2cat",
                   "ie_v1", 
                   "ie_v1_2cat",
                   "ie_2cat_sipecam")
mean(df_aux$ie_2cat_sipecam==df_aux$ie_2cat)
mean(df_aux$ie_2cat_sipecam==df_aux$ie_v1_2cat)
mean(df_aux$ie_2cat==df_aux$ie_v1_2cat)
write_csv(df_aux,
          'data/sipecam_ie.csv')

# Se asume que las observaciones de una especie en un nodo en un mismo día
# son un sólo individuo. Se toma el máximo, ya que puede haber más de un 
# individuo en una sola foto
df <- df %>% 
  filter(!is.na(numero_individuos)) %>% 
  group_by(cumulo, 
           coord_id, coord_longitud, coord_latitud, 
           integridad_nodo,ie,ie_bn,
           fecha_foto, 
           etiqueta) %>% 
  summarise(numero_individuos=max(numero_individuos))

df_aux <- df %>% 
  filter(etiqueta=="Dicotyles tajacu") %>% 
  arrange(coord_id, fecha_foto)

# write_csv(df,
#           'data/counts_sipecam2.csv')
# writeRaster(r_ei, 'data/ie_projected.tif', overwrite=TRUE)

# Sumar todas las observaciones de una especie
# en el módulo íntegro y degradado de cada cúmulo
df_counts <- df %>% 
  group_by(cumulo,
           ie_bn,
           etiqueta) %>% 
  summarise(n=sum(numero_individuos,na.rm = TRUE))
df_counts <- df_counts %>% 
  spread(ie_bn,n) %>% 
  replace(is.na(.), 0) 

# Estimar la diferencia de medias
df_test_integridad_nodo <- get_test_table(df_counts)

df_plot <- df_test_integridad_nodo %>% 
  filter(!is.na(p.value)) %>% 
  mutate(p.value = ifelse(is.na(p.value),0,p.value)) 
ggplot(data=df_plot,
          aes(x=reorder(etiqueta, -estimate), 
                             y=estimate,
                             fill=estimate>=0)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c('lightgreen','red'),
                    labels = c("Íntegro", 
                               "Degradado"),
                    name='Más observaciones en:') +
  # geom_errorbar( aes(x=reorder(etiqueta, -estimate), 
  #                    ymin=conf.low, ymax=conf.high), 
  #                width=0.4, colour="orange", alpha=0.5, size=0.3)+
  geom_text(aes(label=paste0(round(estimate,2))), 
            hjust = 0, size = 2.5,
            position = position_dodge(width = 1),
            inherit.aes = TRUE,
            fontface = ifelse(df_plot %>% 
                                pull(p.value) <= 0.05, 2, 1))+
  ylab('Diferencia de medias (Degradado - Integro)')+
  xlab('')+
  coord_flip()+
  theme_classic()
# ggsave('plot.jpg', dpi=300)
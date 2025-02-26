library(tidyverse)
library(terra)
library(knitr)
library(ggplot2)

source("utils.R")

# Mapa de IE
r_ei <- rast('data/ie/ie_xgb_slic_2021.tif')
r_ei <- project(r_ei, "epsg:4326", method='near')
ecor <- vect('data/ecosistemas_sipecam/Ecosist_Biom_sipecam.shp')

# Datos de foto trampeo
df <- read.csv('data/anotaciones/processed/anotaciones_sipecam_20241016.csv')
df <- df %>% 
  mutate_at(vars(fecha_foto), as.Date, format="%Y-%m-%d") %>% 
  mutate(year = year(fecha_foto))

ecor_cat <- unique(values(ecor['Ecosistema']))
for (i in 1:7){
  print(i)
  ecor_value <- ecor_cat[i,1]
  df_ecor <- df
  
  ecor_selected <- subset(ecor, ecor$Ecosistema == ecor_value)
  ecor_selected <- terra::project(ecor_selected, crs(r_ei))
  
  ie_ecor <- crop(r_ei, ecor_selected, mask=TRUE)
  
  df_coord <- df_ecor %>% 
    select(coord_longitud, coord_latitud)
  df_ecor$ie_value <- terra::extract(ie_ecor, 
                                     df_coord, 
                                     method='exact')$prediction
  df_ecor$ie <- ifelse(df_ecor$ie_value >= 4, 'Degradado', 'Integro')
  df_ecor <- df_ecor %>% 
    drop_na()
  
  
  df_ecor <- df_ecor %>% 
    filter(!is.na(numero_individuos)) %>% 
    group_by(cumulo, 
             coord_longitud, coord_latitud, 
             integridad_nodo,ie,
             fecha_foto, 
             etiqueta) %>% 
    summarise(numero_individuos=max(numero_individuos))
  
  df_counts <- df_ecor %>% 
    group_by(cumulo,
             ie,
             etiqueta) %>% 
    summarise(n=sum(numero_individuos,na.rm = TRUE))
  df_counts <- df_counts %>% 
    spread(ie,n) %>% 
    replace(is.na(.), 0) %>% 
    arrange(etiqueta)
  
  if(!'Degradado' %in% names(df_counts)) {
    df_counts <- df_counts %>% add_column(Degradado = 0)
  } 
  if(!'Integro' %in% names(df_counts)) {
    df_counts <- df_counts %>% add_column(Integro = 0)
  } 
  
  df_test_integridad_nodo <- get_test_table(df_counts)
  df_test_integridad_nodo$ecor <- ecor_value
  
  if(i==1){
    df_output <- df_test_integridad_nodo
  }else{
    df_output <- rbind(df_output,df_test_integridad_nodo)
  }
}

df_output <- df_output %>% 
  mutate(ecor = as.factor(ecor)) %>% 
  mutate(ecor = dplyr::recode(ecor, 
                             '1.-  Bosques templados: coníferas / encino' = 'Bosques templados',
                             '2.-  Bosque mesófilo de montaña' = 'Bosque mesófilo de montaña',
                             '3.-  Selva húmeda: alta/mediana/sub&perennifolia' = 'Selva húmeda',
                             '4.-  Selva seca: sub&caducifolia/espinosa' = 'Selva seca',
                             '5.-  Matorral' = 'Matorral',
                             '6.-  Pastizal' = 'Pastizal',
                             '7.- Vegetación hidrófia' = 'Vegetación hidrófia'))

df_plot <- df_output %>% 
  filter(!is.na(p.value)) # %>% 
# mutate(p.value = ifelse(is.na(p.value),NA,p.value)) 
ggplot(data=df_plot,
       aes(x=reorder(etiqueta, -estimate), 
           y=estimate,
           fill=estimate>=0)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#d7191c",'#1a9641'),
                    labels = c("Degradado", 
                               "Íntegro"),
                    name='Más observaciones en:') +
  geom_text(aes(label=paste0(round(estimate,2))), 
            hjust = 0, size = 2.5,
            position = position_dodge(width = 1),
            inherit.aes = TRUE,
            fontface = ifelse(df_plot %>% 
                                pull(p.value) <= 0.05, 2, 1)) +
  ylab('Diferencia de medias (Íntegro - Degradado)') +
  xlab('') +
  coord_flip() +
  theme_classic() +
  facet_wrap( ~ ecor, nrow = 3)

ecor_selected <- "Bosques templados"
ecor_selected <- "Selva húmeda"
df_plot <- df_output %>% 
  filter(ecor == ecor_selected) %>% 
  filter(!is.na(p.value)) # %>% 
# mutate(p.value = ifelse(is.na(p.value),NA,p.value)) 
ggplot(data=df_plot,
       aes(x=reorder(etiqueta, -estimate), 
           y=estimate,
           fill=estimate>=0)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#d7191c",'#1a9641'),
                    labels = c("Degradado", 
                               "Íntegro"),
                    name='Más observaciones en:') +
  geom_text(aes(label=paste0(round(estimate,2))), 
            hjust = 0, size = 2.5,
            position = position_dodge(width = 1),
            inherit.aes = TRUE,
            fontface = ifelse(df_plot %>% 
                                pull(p.value) <= 0.05, 2, 1)) +
  ylab('Diferencia de medias (Íntegro - Degradado)') +
  xlab('') +
  ggtitle(ecor_selected) +
  coord_flip() +
  theme_classic()
ggsave(paste0("/Users/kasanchez/Documents/ie_model/methods/data/sipecam_",
              ecor_selected,".jpg"))


ecor_selected <- "Bosque mesófilo de montaña"
ecor_selected <- "Selva seca"
df_plot <- df_output %>% 
  filter(ecor == ecor_selected) %>% 
  filter(!is.na(p.value)) # %>% 
# mutate(p.value = ifelse(is.na(p.value),NA,p.value)) 
ggplot(data=df_plot,
       aes(x=reorder(etiqueta, -estimate), 
           y=estimate,
           fill=estimate>=0)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c('#1a9641'),
                    labels = c("Íntegro"),
                    name='Más observaciones en:') +
  geom_text(aes(label=paste0(round(estimate,2))), 
            hjust = 0, size = 2.5,
            position = position_dodge(width = 1),
            inherit.aes = TRUE,
            fontface = ifelse(df_plot %>% 
                                pull(p.value) <= 0.05, 2, 1)) +
  ylab('Diferencia de medias (Íntegro - Degradado)') +
  xlab('') +
  ggtitle(ecor_selected) +
  coord_flip() +
  theme_classic()
ggsave(paste0("/Users/kasanchez/Documents/ie_model/methods/data/sipecam_",
              ecor_selected,".jpg"))

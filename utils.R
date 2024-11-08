library(dplyr)
library(broom)

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
    mutate(estimate = mean(Integro)-mean(Degradado)) %>% 
    ungroup()
  
  df_test1 <- df_ei %>% 
    filter(!(n_samples==n_duplicate)) %>% 
    group_by(etiqueta) %>% 
    do(tidy(t.test(.$Integro,
                   .$Degradado, 
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
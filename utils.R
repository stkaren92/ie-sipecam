# Function that estimates the mean difference between number of
# observations in Integro vs Degradado by specie, running a t-test.
library(dplyr)
library(broom)

get_test_table <- function(df_counts){
  # Estimate by specie the number of samples, 
  # the difference between Integro and Degradado per sample, 
  # and the mean difference
  df_ei <- df_counts %>% 
    group_by(etiqueta) %>% 
    mutate(n_samples=n(), 
           diff = Integro - Degradado,
           estimate = mean(Integro) - mean(Degradado)) %>% 
    ungroup()
  
  # Estimate number of samples with 
  # the same difference by specie
  df_ei <- df_ei %>% 
    group_by(etiqueta,diff) %>% 
    mutate(n_diff_duplicate = n()) %>% 
    ungroup()
  
  # Run a t-test by specie 
  # which samples have multiple difference values
  # so the variance is greater than zero
  df_test1 <- df_ei %>% 
    filter(!(n_samples==n_diff_duplicate)) %>% 
    group_by(etiqueta) %>% 
    do(tidy(t.test(.$Integro,
                   .$Degradado, 
                   mu = 0, 
                   alt = "two.sided", 
                   paired = TRUE, 
                   conf.level = 0.95))) %>% 
    mutate(n=parameter+1)
  
  # Only keep the mean difference for species
  # that didn't have variance greater than zero
  df_test2 <- df_ei %>% 
    filter(!(etiqueta %in% df_test1$etiqueta)) %>% 
    group_by(etiqueta, estimate) %>% 
    summarise(n=n())
  
  df_test <- rbind(df_test1,df_test2)
  return(df_test)
}
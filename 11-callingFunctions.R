source("10-functions.R")
data %>% 
  mutate(long = yards_gained>10) %>% 
  pull(long) %>% 
  mean(na.rm = TRUE)

yards <-  function(x, df) {
  df %>% 
    mutate(long = yards_gained>x) %>% 
    pull(long) %>% 
    mean(na.rm = TRUE)
}

yards(5,data)

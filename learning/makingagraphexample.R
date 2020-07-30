library(tidyverse)

ben <- 
  tibble(
    play = c("two point", "extra point"),
    win = c(0.583, 0.633),
    n = c(101, 96)
  )

theme_set(theme_classic(base_size = 16))

ben %>% 
  ggplot(aes(x = play, y = win, fill = play)) +
  geom_col() +
  geom_text(aes(y = win + 0.03, label = paste0(round(win, 2) * 100, "% ", n, " games"))) +
  scale_fill_manual(values = c("blue", "orange")) + 
  guides(fill = FALSE) +
  labs(x = "", y = "Win probability") +
  ggtitle("My dope plot")+
  theme(plot.title = element_text(hjust = 0.5))
  scale_y_continuous(labels = scales::percent)
  
  
  
round(ben$win, 2)

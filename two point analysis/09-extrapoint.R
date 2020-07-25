extrapoint <- read_csv("data-clean/extrapoint_2009-2019.csv")

#by year
extrapoint %>% count(year)
extrapoint %>% count(year, extra_point_result)
extrapoint %>%
  ggplot() + 
  geom_bar(mapping = aes(x = year, fill = extra_point_result)) +
  scale_fill_manual(values = c("yellow","red" , "#228B22")) +
  labs(title = "Amount of extra points attempted per year between 2009-2019 and success rate")
extrapoint %>%
  ggplot() + 
  geom_bar(mapping = aes(x = year, fill = extra_point_result), position = "fill") +
  scale_fill_manual(values = c("yellow","red" , "#228B22")) +
  labs(title = "Amount of extra points attempted per year between 2009-2019 and success rate")


#by team
extrapoint %>% count(posteam)
extrapoint %>% count(posteam, extra_point_result)
extrapoint %>%
  count(posteam, extra_point_result) %>% 
  mutate(posteam = fct_reorder(posteam, n, sum)) %>% 
  ggplot() +
  geom_col(aes(x=n, y = posteam, fill = extra_point_result)) +
  scale_fill_manual(values = c("yellow","red" , "#228B22")) +
  labs(x = "team",title = "Amount of extra points attempted by each team between 2009-2019 and success rate")
extrapoint %>%
  ggplot() + 
  geom_bar(mapping = aes(x = posteam, fill = extra_point_result), position  = "fill") +
  scale_fill_manual(values = c("yellow","red" , "#228B22")) +
  labs(title = "Amount of extra points attempted per qtr between 2009-2019 and success rate")

#by quarter
extrapoint %>% count(qtr)
extrapoint %>% count(qtr, extra_point_result)
extrapoint %>%
  ggplot() + 
  geom_bar(mapping = aes(x = qtr, fill = extra_point_result)) +
  scale_fill_manual(values = c("yellow","red" , "#228B22")) +
  labs(title = "Amount of extra points attempted per qtr between 2009-2019 and success rate")
extrapoint %>%
  ggplot() + 
  geom_bar(mapping = aes(x = qtr, fill = extra_point_result), position  = "fill") +
  scale_fill_manual(values = c("yellow","red" , "#228B22")) +
  labs(title = "Amount of extra points attempted per qtr between 2009-2019 and success rate")




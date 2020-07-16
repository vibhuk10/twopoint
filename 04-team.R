teams <- twopoint %>% count(posteam)
teams <- arrange(teams, desc(n))
options(tibble.print_max = Inf)
teams

twopoint %>% 
  mutate(posteam = fct_reorder(posteam))
  ggplot() + 
  geom_bar(mapping = aes(x = posteam, fill = two_point_conv_result)) +
  coord_flip() +
  labs(x = "team",title = "Amount of two point conversions attempted by each team between 2009-2019 and success rate")

twopoint %>%
  count(posteam, two_point_conv_result) %>% 
  mutate(posteam = fct_reorder(posteam, n, sum)) %>% 
  ggplot() +
  geom_col(aes(x=n, y = posteam, fill = two_point_conv_result)) +
  scale_fill_manual(values = c("red", "#228B22")) +
  labs(x = "team",title = "Amount of two point conversions attempted by each team between 2009-2019 and success rate")


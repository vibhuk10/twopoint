teams <- twopoint %>% count(posteam)
teams <- arrange(teams, desc(n))
options(tibble.print_max = Inf)
teams
ggplot(data = twopoint) + 
  geom_bar(mapping = aes(x = posteam, fill = two_point_conv_result))


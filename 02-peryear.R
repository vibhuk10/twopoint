twopoint %>% count(year)
ggplot(data = twopoint) + 
  geom_bar(mapping = aes(x = two_point_conv_result, fill = year), position = 'dodge')

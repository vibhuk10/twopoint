twopoint %>% count(year) 
twopoint %>% count(year, two_point_conv_result)
ggplot(data = twopoint) + 
  geom_bar(mapping = aes(x = year, fill = two_point_conv_result))

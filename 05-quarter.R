twopoint %>% count(qtr)
ggplot(data = twopoint) + 
  geom_bar(mapping = aes(x = qtr, fill = two_point_conv_result))
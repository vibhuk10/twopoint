twopoint %>% count(qtr)
ggplot(data = twopoint) + 
  geom_bar(mapping = aes(x = qtr, fill = two_point_conv_result)) +
  labs(x = "quarter", title = "Amount of two point conversions attempted per quarter between 2009-2019 and success rate")

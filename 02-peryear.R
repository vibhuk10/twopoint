twopoint <- read_csv("data-clean/twopoint_2009-2018.csv")
twopoint %>% count(year) 
twopoint %>% count(year, two_point_conv_result)
ggplot(data = twopoint) + 
  geom_bar(mapping = aes(x = year, fill = two_point_conv_result)) +
  labs(title = "Amount of two point conversions attempted per year between 2009-2018 and success rate")

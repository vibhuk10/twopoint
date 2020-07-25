twopoint %>% count(play_type, two_point_conv_result) 
pass_success_rate <- 257/(257+330)
run_success_rate <- 114/(114+86)
pass_success_rate
run_success_rate
ggplot(data = twopoint) + 
  geom_bar(mapping = aes(x = play_type, fill = two_point_conv_result), position = 'fill') +
  labs(x = "play type", title = "Success rate for two point conversions by play type (passing or running) between 2009-2019")

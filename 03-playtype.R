twopoint %>% count(play_type, two_point_conv_result) 
pass_success_rate <- 221/(221+284)
run_success_rate <- 100/(73+100)
pass_success_rate
run_success_rate
ggplot(data = twopoint) + 
  geom_bar(mapping = aes(x = play_type, fill = two_point_conv_result), position = 'fill')

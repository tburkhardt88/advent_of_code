library(tidyverse)

sand_df <- as_tibble(which(sand_grains, arr.ind = T)) %>% setNames(c('y', 'x'))

ggplot(map_df(new_rocks, ~tibble(x = .x[1], y = .x[2])), aes(x, y)) + 
  geom_point(aes(x = 500, y = 0), col = 'blue') +
  geom_tile() + 
  geom_point(data = sand_df, aes(x, y), fill = 'yellow', colour = 'black', pch = 21) +
  scale_y_reverse() + 
  coord_equal() + 
  theme_void() + 
  theme(plot.background = element_rect(fill = 'white', colour = 'white'))

ggsave('part1.png', height = 20, width = 10)

#part 2

sand_df2 <- as_tibble(which(sand_grains2, arr.ind = T)) %>% setNames(c('y', 'x'))

ggplot(map_df(new_rocks, ~tibble(x = .x[1], y = .x[2])), aes(x, y)) + 
  geom_point(aes(x = 500, y = 0), col = 'blue') +
  geom_tile() + 
  geom_point(data = sand_df2, aes(x, y), fill = 'yellow', colour = 'black', pch = 21) +
  scale_y_reverse() + 
  coord_equal() + 
  theme_void() + 
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) + 
  geom_tile(data = tibble(
    x = seq(min(sand_df2$x), max(sand_df2$x)), 
    y = rep(max_y + 2, 345)
  ), aes(x, y))

ggsave('part2.png', height = 10, width = 20)

library(tidyverse)
library(gganimate)

df <- as_tibble(locations) %>% 
  pivot_longer(2:last_col()) %>% 
  mutate(
    ht = str_extract(name, '^.'),
    coord = str_extract(name, '.$')
  ) %>% select(-name) %>% 
  pivot_wider(names_from = coord, values_from = value) 

anim <- ggplot(filter(df, step < 100), aes(x, y)) + 
  geom_point(aes(colour = ht), size = 2) + 
  geom_line(aes(group = step), size = 1) +
  theme_bw() +
  transition_states(
    step,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')

animate(anim)

rope_df <- imap_dfr(rope, function(rp, stp){
  imap_dfr(rp, ~tibble(knot = .y, x = .x[1], y = .x[2])) %>% 
    mutate(step = stp)
})

anim2 <- ggplot(filter(rope_df, step < 500), aes(x, y)) + 
  # geom_point(size = 2) + 
  geom_path(aes(group = step), size = 1) +
  theme_bw() +
  transition_states(
    step,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')


animate(anim2, fps = 3, duration = 60)

anim_save("ten_knot_rope.gif", anim2)


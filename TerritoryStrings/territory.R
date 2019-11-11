# import libraries/preferences/set seed
library(tidyverse)
library(maps)
library(mapproj)
library(scales)
theme_set(theme_light())
set.seed(123)

# abbreviations of all the states/territories
abb = read_csv('abbr.csv', col_names = F) %>% pull()

# this function links them together.
links = function(){
  options = abb
  chain = c()
  # initialize variables and start looping through possibilities.
  while (length(options) > 0){
    step = sample(x = options, size = 1)
    chain = c(chain, step)
    pool = abb[!abb %in% chain]  # exclude items in chain
    options = pool[str_sub(step,2,2) == str_sub(pool,1,1)]  # does the last letter match the first?
  }
  return(chain)
}

links()

# this can be run in parallel because it's drawing randomly each time.
runs = replicate(links(), n = 100000) %>%
  enframe() %>%
  mutate(len = map_int(value, length)) %>%
  mutate(len = len - 1)

# from 100k iterations, the longest chain appears to be 24.
runs %>% arrange(desc(len))


runs %>%
  ggplot() +
  geom_histogram(aes(x=len, fill=len), alpha = .5, bins = max(runs$len)+1)


# Maps --------------------------------------------------------------------

# importing map data
us = map_data("state")

# longest chains of states
longest = runs %>%
  filter(len == max(len)) %>%
  mutate(trimmed = purrr::map(value, str_sub, 1,1)) %>%
  mutate(first_string = purrr::map_chr(trimmed, str_c, collapse = '')) %>%
  mutate(last_letter = str_sub(purrr::map_chr(value, str_c, collapse = ''), -1)) %>%
  mutate(chain = paste0(first_string, last_letter)) %>%
  pull(chain)

# connectivity of given states
conn = runs %>%
  unnest() %>%
  group_by(name = value) %>%
  summarise(avg = mean(len)) %>%
  inner_join(y = tibble(abbr = state.abb, region = tolower(state.name)), by = c('name' = 'abbr'))

# frequency of appearance for given states
appearances = runs %>%
  unnest() %>%
  add_count(value, name = 'count') %>%
  mutate(percent = count / max(name)) %>%
  distinct(value, percent) %>%
  inner_join(y = tibble(abbr = state.abb, region = tolower(state.name)), by = c('value' = 'abbr'))

# mapping connectivity
ggplot() +
  geom_map(data=us, map=us, aes(long, lat, map_id=region), fill=NA, color='grey') +
  geom_map(data=conn, map=us, aes(fill=avg, map_id=region), color='grey', size = .15) +
  coord_map('conic', lat0 = 30) +
  scale_fill_gradient2(name='Average Length', low="white", high="purple") +
  labs(x='Longitude', y='Latitude', title='Connectivity of State Abbreviations',
       caption = 'Average length of path when this state is involved. Territories and non-continguous states excluded.')

# ggsave('maps/connectivity.pdf', width = 9, height = 5)

# mapping appearance
ggplot() +
  geom_map(data=us, map=us, aes(long, lat, map_id=region), fill=NA, color='grey') +
  geom_map(data=appearances, map=us, aes(fill=percent, map_id=region), color='grey', size = .15) +
  coord_map('conic', lat0 = 30) +
  scale_fill_gradient2(name='Percent of Paths', low="white", high="orange", trans = 'sqrt', label = percent) +
  labs(x='Longitude', y='Latitude', title='Appearance of State Abbreviations in Pathways',
       caption = 'Percentage of paths where this state is involved. Territories and non-continguous states excluded.')

# ggsave('maps/appearance.pdf', width = 9, height = 5)



# radial map

pairs = expand.grid(abb, abb, stringsAsFactors = F) %>%
  as_tibble() %>%
  rename(start = Var1, end = Var2) %>%
  filter((str_sub(start,2,2) == str_sub(end,1,1)) & start != end)

circle = abb %>%
  sort() %>%
  enframe(name = 'id', value = 'state') %>%
  mutate(theta = 2 * pi * id / n()) %>%
  mutate(xpos = cos(theta), ypos = sin(theta)) %>%
  select(state, xpos, ypos)

pairs = pairs %>%
  left_join(y = circle, by = c('start' = 'state')) %>% rename(xstart = xpos, ystart = ypos) %>%
  left_join(y = circle, by = c('end' = 'state')) %>% rename(xend = xpos, yend = ypos)


pairs %>%
  ggplot() +
  geom_segment(aes(x=xstart, y=ystart, xend=xend, yend=yend), alpha = .2) +
  geom_text(data=circle, aes(x=xpos, y=ypos, label = state), size = 4.5) +
  labs(x='', y='', title='Connections Between States and Territories') +
  theme_void()

# ggsave('maps/circles.pdf', width = 8, height = 8)

long = runs %>%
  filter(len > 21) %>%
  unnest() %>%
  group_by(name, len) %>%
  transmute(start = value, end = lead(value)) %>%
  na.omit() %>%
  left_join(y = circle, by = c('start' = 'state')) %>% rename(xstart = xpos, ystart = ypos) %>%
  left_join(y = circle, by = c('end' = 'state')) %>% rename(xend = xpos, yend = ypos) %>%
  ungroup() %>%
  mutate(len = factor(len))


pairs %>%
  ggplot() +
  geom_segment(aes(x=xstart, y=ystart, xend=xend, yend=yend), alpha = .1) +
  geom_segment(data=long, aes(x=xstart, y=ystart, xend=xend, yend=yend, color=factor(name)), alpha = .75) +
  geom_text(data=circle, aes(x=xpos, y=ypos, label = state), size = 2.5) +
  scale_color_discrete(name='Length of Runs') +
  facet_wrap(~len) +
  theme_minimal()

# ggsave('maps/circles_lengths.pdf', width = 10, height = 8)


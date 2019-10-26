# import libraries/preferences/set seed
library(tidyverse)
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

# this can be run in parallel because it's drawing randomly each time.
runs = replicate(links(), n = 100000) %>%
  enframe() %>%
  mutate(len = map_int(value, length))

# longest chain appears to be 24.
runs %>% arrange(desc(len))

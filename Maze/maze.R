setwd('~/Desktop/Riddler/Maze/')
library(tidyverse)
library(DiagrammeR)
library(igraph)

grids = tibble(round = as.double(), data = list())
wins = tibble(round = as.double(), data = list())

rid = read_csv('riddler.csv')


# Graphing Time -----------------------------------------------------------

grid = rid

rid2 = expand.grid(rid$id, rid$id) %>%
  as_tibble() %>%
  rename(id1 = Var1, id2 = Var2)

rid2 = rid2 %>%
  left_join(y = rid %>% rename(id1 = id, x1 = x, y1 = y, value1 = value), by = 'id1') %>%
  left_join(y = rid %>% rename(id2 = id, x2 = x, y2 = y, value2 = value), by = 'id2')

rid3 = rid2 %>%
  filter((x1 == x2) | (y1 == y2)) %>%
  mutate(delta = abs((y2 - y1) + (x2 - x1))) %>%
  mutate(begin = value1 == delta, finish = value2 == delta) %>%
  filter(begin | finish)

rid4 = rid3 %>%
  mutate(start = ifelse(begin, id1, id2), end = ifelse(finish, id1, id2)) %>%
  distinct(start, end)


graph = create_graph(nodes_df = create_node_df(n = 100),
                     edges_df = create_edge_df(from = rid4$start, to = rid4$end))

ig = to_igraph(graph)

paths = shortest_paths(graph = ig, from = 91, to = 63, mode = 'out')
path = as.vector(paths$vpath[[1]]) %>% as_tibble() %>% rename(id = value)
path

win = path %>% left_join(y = rid, by = 'id')


# Assessing the Graphs ----------------------------------------------------

grid = grid %>% group_by(round = 1) %>% nest()
win = win %>% group_by(round = 1) %>% nest()

grids = grids %>% bind_rows(grid)
wins = wins %>% bind_rows(win)


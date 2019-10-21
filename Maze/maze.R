library(tidyverse)
library(DiagrammeR)
library(igraph)
theme_set(theme_light())

grids = tibble(round = as.double(), data = list())

rid = read_csv('maze.csv')

rid

gen_grid = function() {
  square = tibble(id = 1:100,
                  x = rep(rep(1:10), 10),
                  y = sort(rep(rep(1:10), 10)),
                  value = sample(x=1:9, size = 100, replace = T))
  return(square)
}


# Graphing Time -----------------------------------------------------------

rid


## So so many problems with mapping ids to each other
# Word of advice to the future avi: draw out a map of how things relate
# other than that, pathfinder works for the example given in the Riddler.

pathfinder = function(grid) {
  grid2 = expand.grid(grid$id, grid$id) %>%
    as_tibble() %>%
    rename(id1 = Var1, id2 = Var2) %>%
    left_join(y = grid %>% rename(id1 = id, x1 = x, y1 = y, value1 = value), by = 'id1') %>%
    left_join(y = grid %>% rename(id2 = id, x2 = x, y2 = y, value2 = value), by = 'id2')

  grid3 = grid2 %>%
    filter((x1 == x2) | (y1 == y2)) %>%
    mutate(delta = abs((y2 - y1) + (x2 - x1))) %>%
    mutate(begin = value1 == delta, finish = value2 == delta) %>%
    filter(begin | finish) %>%
    mutate(start = ifelse(begin, id1, id2), end = ifelse(finish, id1, id2)) %>%
    distinct(start, end)
  
  
  # this has problems for randomly generated grids
  nodes = grid3 %>% gather(cat, node) %>% distinct(node) %>% count() %>% pull()
  graph = create_graph(nodes_df = create_node_df(n = nodes),
                       edges_df = create_edge_df(from = grid3$start, to = grid3$end)) %>% to_igraph()
  
  paths = shortest_paths(graph = graph, from = 91, to = 63, mode = 'out')
  path = as.vector(paths$vpath[[1]]) %>% as_tibble() %>% rename(id = value)
  path
  
  win = path %>% left_join(y = grid, by = 'id')
  
  return(win)
} 



grid4 = grid3 %>% filter(start != end)

ggplot(grid) +
  geom_text(aes(x=x, y=y, label = value)) 

length(unique(c(grid4$start, grid4$end)))

# adding labels keeps track of the mapping between my original ids and those of the nodes created
nodes = create_node_df(length(unique(c(grid4$start, grid4$end))),
                       label = as.character(unique(c(grid4$start, grid4$end))))

nodes

# the problem was that i was using my ids instead of the edge's ids
edges = grid4 %>%
  left_join(y = nodes %>% transmute(label = as.integer(label), start_id = id), by = c('start' = 'label')) %>%
  left_join(y = nodes %>% transmute(label = as.integer(label), end_id = id), by = c('end' = 'label')) %>%
  create_edge_df(from = .$start_id, to = .$end_id)


# thank god this actually works
graph = create_graph(nodes_df = nodes, edges_df = edges) %>% to_igraph()


# the positions given in the Riddler (91 and 63) are my ids, not the graphs, nor the edges.
# i'm still a little shaky on the relation between edges/nodes/graph ids. i need to map this out.
pathing = grid4 %>%
  left_join(y = nodes %>% transmute(label = as.integer(label), start_id = id), by = c('start' = 'label')) %>%
  left_join(y = nodes %>% transmute(label = as.integer(label), end_id = id), by = c('end' = 'label')) %>%
  transmute(launch_id = ifelse(start == 91, start_id, NA), land_id = ifelse(end == 63, end_id, NA)) %>%
  gather(position, value) %>%
  filter(!is.na(value)) %>%
  spread(position, value)

# this extracts the paths
paths = shortest_paths(graph = graph, from = pathing$launch_id, to = pathing$land_id, mode = 'in')
path = as.vector(paths$vpath[[1]]) %>% as_tibble() %>% rename(id = value)
path

edges %>% select(id:to) %>% filter(to == 3)
graph

grid4 %>%
  left_join(y = nodes %>% transmute(label = as.integer(label), start_id = id), by = c('start' = 'label')) %>%
  left_join(y = nodes %>% transmute(label = as.integer(label), end_id = id), by = c('end' = 'label')) %>%
  filter(start_id %in% path | end_id %in% path)


# this visualization can help check that all the path selection is right and not abhorrently wrong
# it's also pretty
grid4 %>% 
  left_join(y = grid %>% transmute(id, start_x = x, start_y = y, value), by = c('start' = 'id')) %>% 
  left_join(y = grid %>% transmute(id, end_x = x, end_y = y), by = c('end' = 'id')) %>%
  filter(start %in% pull(path) | end %in% pull(path)) %>%
  ggplot() +
  geom_curve(aes(x=start_x, xend = end_x, y = start_y, yend = end_y, color=factor(value)), alpha=.5,size=1.5) +
  geom_text(data=grid, aes(x=x, y=y, label = value), size = 7) +
  lims(x=c(-2,12), y=c(-2,12)) +
  scale_color_discrete(guide=F)


#

# yeah so this is the ideal state that we'll get to eventually
# then it's just running these a bunch of times
solved = pathfinder(rid)

# test = gen_grid()




# Assessing the Graphs ----------------------------------------------------

# one day i will get here
# that day is not today
grids = grids %>% bind_rows(solved %>% group_by(round = 1) %>% nest())

grids

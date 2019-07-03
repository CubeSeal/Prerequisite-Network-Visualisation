# Packages
library(dplyr)
library(tidyr)
library(tidygraph)
library(ggraph)
library(stringr)
library(reshape2)

# Mac Uni Data commented out
# macq_units.df <- read.csv("macquarie-unsw.csv", na.strings = "null", stringsAsFactors = F)
# 
# named_edges <- macq_units.df %>%
#     na.omit() %>%
#     mutate(source = str_extract(unit.codes, "[A-Z]{3,4}[0-9]{3,4}")) %>%
#     select(source, destination=prereqs)

# UNSW Data
unsw_courses.df <- read.csv("unsw-courses.csv", na.strings = "null",
                            stringsAsFactors = F)

# DF of prereq links and faculty wrangled from raw data
named_edges <- unsw_courses.df %>%
    na.omit() %>%
    mutate(destination = str_extract(courses, "[A-Z]{4}\\d{4}(?!\\d)")) %>%
    mutate(prereqs = str_extract_all(conditions, "[A-Z]{4}\\d{4}(?!\\d)")) %>%
    unnest(prereqs) %>%
    distinct() %>%
    select(source = prereqs, destination, faculties)

# igraph DF of nodes (id, label and faculty)
#   Also filters by subject code or faculty
nodes <- melt(named_edges, id.vars = 'faculties') %>%
    select(label = value, faculties) %>%
    unique() %>%
    filter(str_detect(faculties, "Law")) %>%
    mutate(id = 1:n()) %>%
    select(id, label, faculties)

# igraph DF of edges by node id
edges <- named_edges %>%
    inner_join(nodes, by = c("source" = "label")) %>%
    rename(from = id) %>%
    inner_join(nodes, by = c("destination" = "label")) %>%
    rename(to = id) %>%
    select(from, to)

# Tidygraph routes data from nodes and edges
routes <- tbl_graph(nodes = nodes, edges = edges, directed = T)

# Plot the routes data
ggraph(routes,
       # Uses first number of subj code as layer number.
       # layers = as.numeric(str_extract(nodes$label, "\\d{1}")),
       layout = "sugiyama",
       hgap = 4,
       maxiter = 10000) +
    geom_edge_diagonal(alpha = 0.1,
                       colour = 'white') +
    geom_node_point(size = 1.5, aes(colour = faculties)) +
    theme_graph() +
    theme(
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        legend.text = element_text(colour = "white"),
        legend.title = element_text(colour = "white")
    ) +
    scale_colour_brewer(palette = "Set1")


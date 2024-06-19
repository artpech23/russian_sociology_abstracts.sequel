## author: Arthur Pecherskikh (apecherskikh@eu.spb.ru)
## code to reproduce the analysis of the editorial board interlocking (EBI) network of Russian sociological journals

################
## libraries: ##
################

library(tidyverse)
library(igraph)
library(intergraph)
library(netUtils)

#################
## load files: ##
#################

vertices <- read.csv("editorial.boards_vertices.csv")
edges <- read.csv("editorial.boards_edges.csv")

##########################################
## get bimodal network and projections: ##
##########################################

two.mode <- graph_from_data_frame(edges %>%
                                    ## get unique journal-editor couples:
                                    count(journal_name, 
                                          unified_name),
                                  directed = F)

## assign bipartite mapping:
V(two.mode)$type <- bipartite_mapping(two.mode)$type

## extract projections:
bipartite_matrix <- as_biadjacency_matrix(two.mode)
journal_matrix <- bipartite_matrix %*% t(bipartite_matrix)

## create journals' projection:
diag(journal_matrix) <- 0

g_journals <- graph_from_adjacency_matrix(journal_matrix,
                                          mode = "undirected",
                                          weighted = TRUE)

rm(bipartite_matrix, journal_matrix, two.mode)

##############################
## get degree centralities: ## 
##############################

V(g_journals)$degree = degree(g_journals, mode = "total")

## preview the most central journals:
(g_journals %>% asDF())$vertexes %>% 
  select(-intergraph_id) %>% 
  arrange(desc(degree)) %>% 
  head()

##########################
## community detection: ##
##########################

set.seed(42)
cl3 <- cluster_louvain(g_journals,
                       resolution = 0.75)

V(g_journals)$cl3 = membership(cl3)

## solution preview:
set.seed(42)
plot(g_journals,
     vertex.color = ifelse(V(g_journals)$cl3 == 1,
                           "#988F2A",
                           ifelse(V(g_journals)$cl3 == 2,
                                  "coral1",
                                  "#30638E")),
     vertex.label = NA,
     vertex.size = V(g_journals)$degree * 0.6)

rm(cl3)

#####################
## core/periphery: ##
#####################

core_peiphery <- netUtils::core_periphery(g_journals)
V(g_journals)$core_periphery = core_peiphery$vec

## solution preview:
set.seed(42)
plot(g_journals,
     vertex.color = ifelse(V(g_journals)$core_periphery == 1,
                           "lightgreen",
                           "coral1"),
     vertex.label = NA,
     vertex.size = V(g_journals)$degree * 0.6)

rm(core_periphery)

##############################################
## table of core/periphery and communities: ##
##############################################

journals_positions <- (asDF(g_journals))$vertexes %>% 
  rename(journal_name = name) %>% 
  arrange(desc(degree)) %>% 
  select(-intergraph_id, -degree)

## table preview:
head(journals_positions)

###############################
## get community properties: ##
###############################

communities <- (asDF(g_journals))$vertexes %>% 
  select(-intergraph_id) %>% 
  rename(journal_name = name) %>% 
  
  left_join(vertices, by = "journal_name") %>%
  mutate(cl3 = ifelse(cl3 == 1,
                      "west",
                      ifelse(cl3 == 2,
                             "east",
                             "center"))) %>% 
  
  group_by(cl3) %>% 
  summarise(mean_reference_list = mean(mean_n_references2021),
            mean_publications = mean(total_publications2021),
            mean_citations = mean(total_citations2021),
            
            share_foreigners = mean(share_foreigners),
            share_RAS = mean(share_ras),
            share_moscowites = mean(share_moscowite)
            ## more attributes can be calculated here. I include those which are reported among the picture.
            )

## properties' preview:
communities[1:3,1:4]

#######################
## reduced networks: ##
#######################

## Here, you need to run par() and loop together. To get the reduced graphs on the same picture.
## You may also need to zoom to see them (this is due to RStusio default view properties).

par(mfrow = c(2,2))

for(i in c(0:3)){

g <- g_journals %>%
  as_data_frame() %>%
  filter(weight > i) %>%
  graph_from_data_frame(directed = F)

V(g)$color <-((asDF(g))$vertexes %>% 
                 
                 left_join(journals_positions %>% 
                             rename(name = journal_name)) %>%
                 mutate(cl_colors = ifelse(cl3 == 1,
                                          "#988F2A",
                                          ifelse(cl3 == 2,
                                                 "coral1",
                                                 "#30638E"))))$cl_colors
plot(g,
     vertex.label = NA,
     main = str_c("сила связи > ", i))
}


###################
## session Info: ##
###################

# Description below created via sessionInfo() %>% report::report()

# Analyses were conducted using the R Statistical language (version 4.3.1; R Core Team, 2023) on Windows 10 x64 (build 19045), using the packages:

# intergraph  (version 2.0.4; Bojanowski M, 2023),
# igraph      (version 2.0.2; Csardi G, Nepusz T, 2006),
# lubridate   (version 1.9.3; Grolemund G, Wickham H, 2011),
# tibble      (version 3.2.1; Müller K, Wickham H, 2023),
# netUtils    (version 0.8.2; Schoch D, 2023),
# ggplot2     (version 3.5.1; Wickham H, 2016),
# forcats     (version 1.0.0; Wickham H, 2023),
# stringr     (version 1.5.1; Wickham H, 2023),
# tidyverse   (version 2.0.0; Wickham H et al., 2019),
# dplyr       (version 1.1.2; Wickham H et al., 2023),
# purrr       (version 1.0.2; Wickham H, Henry L, 2023),
# readr       (version 2.1.5; Wickham H et al., 2024) and 
# tidyr       (version 1.3.1; Wickham H et al., 2024).
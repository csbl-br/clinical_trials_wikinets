library(dplyr)
library(data.table)
library(tidyverse)
library(igraph)
library(ggraph)

#Load edges and nodes
edges <- data.table::fread("data/edges_dis_interventions.tsv")
colnames(edges) <- c("weight", "from", "to")
edges <- edges[, c(2, 3, 1)]

#Remove COVID19 from "interventions"
covid19_id <- "<http://www.wikidata.org/entity/Q84263196>"

edges <- edges %>%
  dplyr::filter(to != covid19_id)

disease_nodes <- data.table::fread("data/disease_nodes.tsv")
colnames(disease_nodes) <- c("URL", "label")
disease_nodes$class <- "disease"

intervention_nodes <- data.table::fread("data/intervention_nodes.tsv")
colnames(intervention_nodes) <- c("URL", "label")
intervention_nodes$class <- "intervention"
intervention_nodes <- intervention_nodes %>%
  filter(URL != covid19_id)

all_nodes <- rbind(disease_nodes, intervention_nodes) %>%
  dplyr::mutate(label = gsub(
    x = label,
    pattern = '"|@en',
    replacement = ""
  ))

#Create first graph
g1 <- igraph::graph_from_data_frame(d = edges, directed = F)

all_nodes_sorted <- data.frame(URL=V(g1)$name) %>%
  left_join(all_nodes,by = "URL")

V(g1)$degree <- degree(graph = g1)
all_nodes_sorted$degree <- V(g1)$degree
V(g1)$class <- all_nodes_sorted$class
louv <- cluster_louvain(g1,weights = E(g1)$weight)
V(g1)$mod <- louv$membership

all_nodes_sorted$mod <- louv$membership

#Plot the network
# p <- ggraph::ggraph(graph = g1,layout = "kk")+
#   ggraph::geom_edge_link()+
#   ggraph::geom_node_point()+
#   theme_void()
# p

#Plot top10 disease and interventions
p <- all_nodes_sorted %>%
  group_by(class) %>%
  top_n(n = 20,wt = degree) %>%
  arrange(desc(degree)) %>%
  ungroup() %>%
  mutate(label=factor(label,levels = unique(label))) %>%
  ggplot(aes(x=label,y=degree,fill=class))+
  geom_col()+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90,hjust = 1,size = 15))+
  facet_wrap(facets = ~class,scales = "free_x",nrow = 2)
pdf(file = "figures/top20_diseases_and_interventions.pdf",
    width = 8,height = 10)
print(p)  
dev.off()



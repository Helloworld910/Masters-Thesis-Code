#Importing Libraries
library(tidyverse)
library(udpipe)
library(ggtext)
library(magrittr)
library(igraph)
library(ggraph)
library(flextable)

#Setting Root Directory, Adding Font
setwd("E:/Charles University/Thesis/masters-thesis-code")
rm(list=ls())
windowsFonts(georgia = windowsFont("georgia"))

#Upload Data
citation_df <- read_csv("data/citation_data.csv")

#Check for Missing Values
citation_df %<>%
  filter(!is.na(cited_author))

#Count Occurrences of Citation
citation_df$both_side <- paste0(citation_df$author,"_SEP_", citation_df$cited_author)
citation_df <- citation_df %>%
  count(both_side, sort=TRUE)
citation_df %<>%
  separate(col = both_side, into = c("from", "to"), sep = "_SEP_")

#Recoding Laasch, O. to Laasch, Oliver
citation_df$to_recoded <- ifelse(citation_df$to == "Laasch, O.", "Laasch, Oliver", citation_df$to)
citation_df %<>%
  select(from,to_recoded,n)

#Converting to iGraph Object
cit_net <- graph_from_data_frame(citation_df)

#Checking if Citation Network is Connected
is.connected(cit_net)

#Extracting Degree Distribution
degs <- degree(cit_net, mode = "all")


#Plotting Degree Distribution
degs <- tibble(degree = degs)

degs %>%
  count(degree)%>%
  ggplot()+
  geom_line(aes(x = n, y = degree))+
  labs(x = "Frequency", y = "Degree") +
  theme_classic() +
  theme(
    axis.text.x = element_text(family = "georgia", angle = 0, hjust = 1),
    axis.text.y = element_text(family = "georgia"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "#0D0D0D"),
    legend.position = "none",
    plot.title = element_textbox(face = "bold", family = "georgia", size = 23, hjust = 0.5),
    axis.title = element_textbox(face = "bold", family = "georgia", size = 11),
    axis.text = element_textbox(size = 13),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "#F0F0F2"),
    plot.background = element_rect(colour = "#0D0D0D", size = 1),
  ) +
  ggtitle("Degree Distribution")

#Extracting Diameter
diameter <- diameter(cit_net, directed = TRUE, unconnected = TRUE)

#Extracting Clustering Coefficient
clus_coef <- transitivity(cit_net, type = "global")


#Extracting Average Distance between Two Nodes
mean_dist <- mean_distance(cit_net,  details=FALSE)


#Creating Table of Descriptive Stats
desc_stats <- tibble(names = c("Diameter", "Clustering Coefficient", "Average Distance"),
                     statistic = c(diameter, clus_coef, mean_dist))

#Exporting Table of Descriptive Stats
my_flextable <- flextable(desc_stats)
my_flextable <- set_table_properties(my_flextable, width = 1)
save_as_docx(my_flextable, path = "figures_and_tables/table.4.1.docx")



#Extracting Major Hubs
degs <- degree(cit_net, mode = "all")
cutoff <- quantile(degs, 0.95)
top_nodes <- which(degs >= cutoff)
major_hubs <- induced_subgraph(cit_net, top_nodes)

vcount(major_hubs)
ecount(major_hubs)


#Finding Communities
members <- cluster_walktrap(major_hubs)

#Assigning Communities
V(major_hubs)$community <- members$membership


#Drawing the Major Hubs
set.seed(358)
ggraph(major_hubs, layout="fr") +
  geom_node_point(aes(color = as_factor(community)), alpha = 1, size = 2) +
  geom_edge_link(edge_alpha = 0.1, width = 1)+
  ggtitle("Diagram of the Major Hubs - Citation Network") +
  theme(plot.title = element_textbox(face = "bold", color = "#0D0D0D", 
                                     family = "georgia", size = , hjust = 0.5),
        legend.position = "none",
        plot.background = element_rect(color = "#0D0D0D", size = 1),
        panel.background = element_rect(fill = "#F0F0F2"))


#Coding Active Members of Discourse as 1 and Non-Active as 0
active_authors <- citation_df$from
active_authors <- unique(active_authors)

for (i in 1:vcount(cit_net)) {
  if (V(cit_net)$name[i] %in% active_authors) {
    V(cit_net)$active[i] <- 1
  }
  else {
    V(cit_net)$active[i] <- 0
  }
}


#Drawing Active Inactive Diagram
set.seed(358)
ggraph(cit_net, layout="lgl") +
  geom_edge_link(edge_alpha = 0.1, width = 1)+
  geom_node_point(aes(color = as_factor(active)), alpha = 1, size = 3) +
  ggtitle("Active-Inactive Diagram of Citation Network") +
  labs(color = "Active/Inactive")+
  scale_color_manual(labels=c("Inactive","Active"), values=c("#AED2ED","#F6776B"))+
  theme(plot.title = element_textbox(face = "bold", color = "#0D0D0D", 
                                     family = "georgia", size = , hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_textbox(face = "bold", color = "#0D0D0D", 
                                       family = "georgia", size = 13),
        legend.text = element_textbox(face = "bold", color = "#0D0D0D",
                                      family = "georgia", size = 11),
        plot.background = element_rect(color = "#0D0D0D", size = 1),
        panel.background = element_rect(fill = "#F0F0F2"))



#Extracting Largest Component
components <- decompose(cit_net)
largest_component <- components[[1]]



#Extracting Sub-graph of Top 10 Nodes with Highest Eigenvector Centrality Scores
eigens_author <- eigen_centrality(largest_component, directed = TRUE)
eigens_author <- eigens_author$vector
top_eigens <- order(eigens_author, decreasing = TRUE)[1:10]
top_author_eigens <- induced_subgraph(cit_net, top_eigens)


#Drawing Sub-graph of top 10 Nodes With Highest Eigenvector Centrality
set.seed(358)
ggraph(top_author_eigens, layout="circle") +
  geom_edge_link(aes(alpha = n), width = 2, colour = "#8C8C8C", show.legend=FALSE) +
  geom_node_point(aes(color = as_factor(active)), size = 5)+
  labs(color = "Active/Inactive")+
  scale_color_manual(labels=c("Inactive","Active"), values=c("#AED2ED","#F6776B"))+
  ggtitle("Induced Subgraph of 10 Highest Eigenvector Centrality Authors") +
  labs(color = "Active/Inactive")+
  theme(plot.title = element_textbox(face = "bold", color = "#0D0D0D", 
                                     family = "georgia", size = , hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_textbox(face = "bold", color = "#0D0D0D", 
                                       family = "georgia", size = 13),
        legend.text = element_textbox(face = "bold", color = "#0D0D0D", 
                                      family = "georgia", size = 11),
        plot.background = element_rect(color = "#0D0D0D", size = 1),
        panel.background = element_rect(fill = "#F0F0F2"))


#Creating Table of Top 10 Eigenvector Centrality Nodes
author_top_eigens <- names(eigens_author)[order(eigens_author, decreasing = TRUE)[1:10]]
value_top_eigens <- unname(eigens_author)[order(eigens_author, decreasing = TRUE)[1:10]]

top_degree_table <- tibble(Author = author_top_eigens, eigenvector_centrality = value_top_eigens)


top_degree_table$Active <- ifelse(top_degree_table$Author %in% active_authors, "Active", "Inactive")


#Exporting Table of Eigenvector Centrality
my_flextable <- flextable(top_degree_table)
my_flextable <- set_table_properties(my_flextable, width = 1)
save_as_docx(my_flextable, path = "figures_and_tables/table.4.2.docx")


#Plotting Distribution of Eigenvector Centrality
eigen_centrality_dist <- tibble(author = names(eigens_author), eig_cent = unname(eigens_author))

eigen_centrality_dist %>%
  ggplot()+
  geom_freqpoly(aes(x=eig_cent))+
  labs(y = "Frequency", x = "Eigenvector Centrality") +
  theme_classic() +
  theme(
    axis.text.x = element_text(family = "georgia", angle = 0, hjust = 1),
    axis.text.y = element_text(family = "georgia"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "#0D0D0D"),
    legend.position = "none",
    plot.title = element_textbox(face = "bold", family = "georgia", size = 23, hjust = 0.5),
    axis.title = element_textbox(face = "bold", family = "georgia", size = 11),
    axis.text = element_textbox(size = 13),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "#F0F0F2"),
    plot.background = element_rect(colour = "#0D0D0D", size = 1),
  ) +
  ggtitle("Eigenvector Centrality Distribution")

################################################################################
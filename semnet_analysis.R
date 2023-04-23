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

#Uploading Data as Tibble
anno_df <- read_csv("data/annotated_df.csv")

#Inspecting the Uploaded Data
names(anno_df)

#Creating Co-occurrences Tibble
cooc <- cooccurrence(x = subset(anno_df, upos %in% c("NOUN", "ADJ", "VERB")), 
                     term = "lemma", 
                     group = c("doc_id", "paragraph_id", "sentence_id"))

cooc <- as_tibble(cooc)

#Extracting Semantic Network
sem_net <- graph_from_data_frame(cooc)
rm(cooc)
gc()

#Extracting Degree Distribution
degs <- degree(sem_net, mode = "all")


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
diameter <- diameter(sem_net, directed = TRUE, unconnected = TRUE)

#Extracting Clustering Coefficient
clus_coef <- transitivity(sem_net, type = "global")




#Extracting Average Distance between Two Nodes
mean_dist <- mean_distance(sem_net,  details=FALSE)

#Creating Table of Descriptive Stats
desc_stats <- tibble(names = c("Diameter", "Clustering Coefficient", "Average Distance"),
                     statistic = c(diameter, clus_coef, mean_dist))

#Exporting Table of Descriptive Stats
my_flextable <- flextable(desc_stats)
my_flextable <- set_table_properties(my_flextable, width = 1)
save_as_docx(my_flextable, path = "figures_and_tables/table.2.1.docx")


#Extracting Major Hubs
degs <- degree(sem_net, mode = "all")
cutoff <- quantile(degs, 0.95)
top_nodes <- which(degs >= cutoff)
major_hubs <- induced_subgraph(sem_net, top_nodes)


#Trimming Major Hubs Graph
major_hubs_trimmed <- delete_edges(major_hubs, E(major_hubs)[cooc < 100])

vcount(major_hubs_trimmed)
ecount(major_hubs_trimmed)

#Drawing the Major Hubs
set.seed(358)
ggraph(major_hubs, layout="fr") +
  geom_node_point(color = "#262626", alpha = 0.7) +
  geom_edge_link(edge_colour = "#8C8C8C", edge_alpha = 1) +
  ggtitle("Diagram of the Major Hubs") +
  theme(plot.title = element_textbox(face = "bold", color = "#0D0D0D", 
                                     family = "georgia", size = , hjust = 0.5),
        legend.position = "none",
        plot.background = element_rect(color = "#0D0D0D", size = 1),
        panel.background = element_rect(fill = "#F0F0F2"))


#Creating Sub-graph of Nouns
sub_noun <- cooccurrence(x = subset(anno_df, upos %in% c("NOUN")), 
                     term = "lemma", 
                     group = c("doc_id", "paragraph_id", "sentence_id"))
sub_noun <- as_tibble(sub_noun)
sub_noun <- graph_from_data_frame(sub_noun)

#Extracting Top 10 Degree Nodes
degs_noun <- degree(sub_noun, mode = "all")
top_degrees <- order(degs_noun, decreasing = TRUE)[1:10]
top_nouns_degree <- induced_subgraph(sub_noun, top_degrees)


#Drawing Sub-graph of top 10 Nodes With Highest Degree Centrality
set.seed(358)
ggraph(top_nouns_degree, layout="circle") +
  geom_edge_link(aes(alpha = cooc), width = 2, colour = "#8C8C8C") +
  geom_node_label(aes(label = name), color = "#0D0D0D", size = 5, family="georgia")+
  
  ggtitle("Induced Subgraph of 10 Highest Degree Centrality Nodes") +
  theme(plot.title = element_textbox(face = "bold", color = "#0D0D0D", 
                                     family = "georgia", size = , hjust = 0.5),
        legend.position = "none",
        plot.background = element_rect(color = "#0D0D0D", size = 1),
        panel.background = element_rect(fill = "#F0F0F2"))


#Checking the Presence of P.
what_is_p <- anno_df %>%
  filter(lemma=="p.") %>%
  select(sentence) %>%
  slice(1:3)

what_is_p[[1, "sentence"]]
what_is_p[[2, "sentence"]]
what_is_p[[3, "sentence"]]


#Creating Table of Top 10 Degree Centrality Nodes
lemma_top_deg <- names(V(sub_noun))[top_degrees]
value_top_deg <- degree(sub_noun, v = top_degrees, mode="all")
top_degree_table <- tibble(lemma = lemma_top_deg, degree = value_top_deg)

#Exporting Table of Degree Centrality
my_flextable <- flextable(top_degree_table)
my_flextable <- set_table_properties(my_flextable, width = 1)
save_as_docx(my_flextable, path = "figures_and_tables/table.2.2.docx")


#Checking if Sub-graph of Nouns is Connected
is.connected(sub_noun)

#Extracting Largest Component
components <- decompose(sub_noun)
largest_component <- components[[1]]



#Extracting Sub-graph of Top 10 Nodes with Highest Eigenvector Centrality Scores
eigens_noun <- eigen_centrality(largest_component, directed = FALSE)
eigens_noun <- eigens_noun$vector
top_eigens <- order(eigens_noun, decreasing = TRUE)[1:10]
top_nouns_eigens <- induced_subgraph(sub_noun, top_eigens)


#Drawing Sub-graph of top 10 Nodes With Highest Eigenvector Centrality
set.seed(358)
ggraph(top_nouns_eigens, layout="circle") +
  geom_edge_link(aes(alpha = cooc), width = 2, colour = "#8C8C8C") +
  geom_node_label(aes(label = name), color = "#0D0D0D", size = 5, family="georgia")+
  
  ggtitle("Induced Subgraph of 10 Highest Eigenvector Centrality Nodes") +
  theme(plot.title = element_textbox(face = "bold", color = "#0D0D0D", 
                                     family = "georgia", size = , hjust = 0.5),
        legend.position = "none",
        plot.background = element_rect(color = "#0D0D0D", size = 1),
        panel.background = element_rect(fill = "#F0F0F2"))


#Creating Table of Top 10 Eigenvector Centrality Nodes
lemma_top_eigens <- names(eigens_noun)[order(eigens_noun, decreasing = TRUE)[1:10]]
value_top_eigens <- unname(eigens_noun)[order(eigens_noun, decreasing = TRUE)[1:10]]

top_degree_table <- tibble(lemma = lemma_top_eigens, eigenvector_centrality = value_top_eigens)

#Exporting Table of Eigenvector Centrality
my_flextable <- flextable(top_degree_table)
my_flextable <- set_table_properties(my_flextable, width = 1)
save_as_docx(my_flextable, path = "figures_and_tables/table.2.3.docx")


#Extracting the Distribution of the Parts of Speeches for Posthuman
pos_dis_phum <- anno_df %>%
  filter(lemma=="posthuman")%>%
  count(upos)

#Assigning Full Forms
abbr <- c("ADJ", "ADP", "ADV", "AUX", "CCONJ", "DET", "INTJ", "NOUN", "NUM", "PART", "PRON", "PROPN", "PUNCT", "SCONJ", "SYM", "VERB", "X")
full <- c("Adjective", "Adposition", "Adverb", "Auxiliary", "Coordinating Conjunction", "Determiner", "Interjection", "Noun", "Numeral", "Particle", "Pronoun", "Proper Noun", "Punctuation", "Subordinating Conjunction", "Symbol", "Verb", "Other")
tib_full <- tibble(abbr, full)

#Joining Full Forms
pos_dis_phum %<>%
  left_join(tib_full, by = c("upos"="abbr")) %>%
  select(upos,full,n)

#Plotting POS Distribution for Posthuman
pos_dis_phum %>%
  ggplot() +
  geom_col(aes(x = n, y = reorder(full,n)), fill = "#262626")+
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE))+
  labs(x = "Frequency", y = "Parts of Speech")+
  theme_classic()+
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
  ggtitle("Distribution of Parts of Speech for Posthuman")


#Extracting Node Statistics of Posthuman
phum_deg <- unname(degree(sem_net, v="posthuman", mode="all"))
phum_clus_coef <- transitivity(sem_net, vid="posthuman", type = "barrat")
components <- decompose(sem_net)
largest_component <- components[[1]]
eigens_noun <- eigen_centrality(largest_component, directed = FALSE)
eigens_noun <- eigens_noun$vector
phum_eigen_cent <- unname(eigens_noun["posthuman"])
rm(components)
rm(largest_component)
rm(eigens_noun)

#Creating Table of Descriptive Stats
ph_desc_stats <- tibble(names = c("Degree", "Eigenvector Centrality Score", "Local Clustering Coefficient"),
                     statistic = c(phum_deg, phum_eigen_cent, phum_clus_coef))

#Exporting Table of Descriptive Stats
my_flextable <- flextable(ph_desc_stats)
my_flextable <- set_table_properties(my_flextable, width = 1)
save_as_docx(my_flextable, path = "figures_and_tables/table.3.1.docx")


#Trimming Semantic Network for Posthuman Ego Network Extraction
sem_net_trimmed <- delete_edges(sem_net, E(sem_net)[cooc < 70])


#Extracting the Ego-Network of Posthuman
ego_network <- ego(sem_net_trimmed, node = "posthuman", order=1, mode="all")
ego_network <- ego_network[[1]]
induced_ego <- induced_subgraph(sem_net_trimmed, ego_network)


vcount(induced_ego)
ecount(induced_ego)


#Finding Communities in Ego Network
members <- cluster_walktrap(induced_ego)

#Assigning Communities
V(induced_ego)$community <- members$membership


#Drawing Ego Network with Communities
set.seed(358)
ggraph(induced_ego, layout="fr") +
  geom_edge_link2(aes(alpha = cooc,colour = as_factor(node.community)), arrow = arrow(length = unit(4, 'mm')), 
                  end_cap = circle(5, 'mm'), width = 2, show.legend = FALSE)+
  geom_node_text(aes(label = name), colour = "#0D0D0D", size = 5, family="georgia")+
  ggtitle("Degree 1 Ego Network of Posthuman") +
  theme(plot.title = element_textbox(face = "bold", color = "#0D0D0D", 
                                     family = "georgia", size = , hjust = 0.5),
        
        plot.background = element_rect(color = "#0D0D0D", size = 1),
        panel.background = element_rect(fill = "#F0F0F2"))
 

#Trimming Semantic Network to Extract Ego Network of Work
sem_net_trimmed <- delete_edges(sem_net, E(sem_net)[cooc < 300])


#Extracting the Ego-Network of Work
ego_network <- ego(sem_net_trimmed, node = "work", order=1, mode="all")
ego_network <- ego_network[[1]]
induced_ego <- induced_subgraph(sem_net_trimmed, ego_network)


vcount(induced_ego)
ecount(induced_ego)


#Finding Communities in Ego Network
members <- cluster_walktrap(induced_ego)
#Assigning Communities
V(induced_ego)$community <- members$membership


#Drawing Ego Network with Communities
set.seed(358)
ggraph(induced_ego, layout="fr") +
  geom_edge_link2(aes(alpha = cooc,colour = as_factor(node.community)), arrow = arrow(length = unit(4, 'mm')), 
                  end_cap = circle(5, 'mm'), width = 2, show.legend = FALSE)+
  geom_node_text(aes(label = name), colour = "#0D0D0D", size = 5, family="georgia")+
  ggtitle("Degree 1 Ego Network of Work") +
  theme(plot.title = element_textbox(face = "bold", color = "#0D0D0D", 
                                     family = "georgia", size = , hjust = 0.5),
        
        plot.background = element_rect(color = "#0D0D0D", size = 1),
        panel.background = element_rect(fill = "#F0F0F2"))


#Trimming Semantic Network to Extract Ego Network of Body
sem_net_trimmed <- delete_edges(sem_net, E(sem_net)[cooc < 250])


#Extracting the Ego-Network of Body
ego_network <- ego(sem_net_trimmed, node = "body", order=1, mode="all")
ego_network <- ego_network[[1]]
induced_ego <- induced_subgraph(sem_net_trimmed, ego_network)


vcount(induced_ego)
ecount(induced_ego)

#Finding Communities in Ego Network
members <- cluster_walktrap(induced_ego)

#Assigning Communities
V(induced_ego)$community <- members$membership

#Drawing Ego Network with Communities
set.seed(358)
ggraph(induced_ego, layout="fr") +
  geom_edge_link2(aes(alpha = cooc,colour = as_factor(node.community)), arrow = arrow(length = unit(4, 'mm')), 
                  end_cap = circle(5, 'mm'), width = 2, show.legend = FALSE)+
  geom_node_text(aes(label = name), colour = "#0D0D0D", size = 5, family="georgia")+
  ggtitle("Degree 1 Ego Network of Body") +
  theme(plot.title = element_textbox(face = "bold", color = "#0D0D0D", 
                                     family = "georgia", size = , hjust = 0.5),
        
        plot.background = element_rect(color = "#0D0D0D", size = 1),
        panel.background = element_rect(fill = "#F0F0F2"))


#Trimming Semantic Network to Extract Ego Network of Other
sem_net_trimmed <- delete_edges(sem_net, E(sem_net)[cooc < 500])

#Extracting the Ego-Network of Other
ego_network <- ego(sem_net_trimmed, node = "other", order=1, mode="all")
ego_network <- ego_network[[1]]
induced_ego <- induced_subgraph(sem_net_trimmed, ego_network)


vcount(induced_ego)
ecount(induced_ego)

#Finding Communities in Ego Network
members <- cluster_walktrap(induced_ego)

#Assigning Communities
V(induced_ego)$community <- members$membership

#Drawing Ego Network with Communities
set.seed(358)
ggraph(induced_ego, layout="fr") +
  geom_edge_link2(aes(alpha = cooc,colour = as_factor(node.community)), arrow = arrow(length = unit(4, 'mm')), 
                  end_cap = circle(5, 'mm'), width = 2, show.legend = FALSE)+
  geom_node_text(aes(label = name), colour = "#0D0D0D", size = 5, family="georgia")+
  ggtitle("Degree 1 Ego Network of Other") +
  theme(plot.title = element_textbox(face = "bold", color = "#0D0D0D", 
                                     family = "georgia", size = , hjust = 0.5),
        
        plot.background = element_rect(color = "#0D0D0D", size = 1),
        panel.background = element_rect(fill = "#F0F0F2"))



#Trimming Semantic Network to Extract Ego Network of Human
sem_net_trimmed <- delete_edges(sem_net, E(sem_net)[cooc < 800])

#Extracting the Ego-Network of Human
ego_network <- ego(sem_net_trimmed, node = "human", order=1, mode="all")
ego_network <- ego_network[[1]]
induced_ego <- induced_subgraph(sem_net_trimmed, ego_network)


vcount(induced_ego)
ecount(induced_ego)

#Finding Communities in Ego Network
members <- cluster_walktrap(induced_ego)

#Assigning Communities
V(induced_ego)$community <- members$membership

#Drawing Ego Network with Communities
set.seed(358)
ggraph(induced_ego, layout="fr") +
  geom_edge_link2(aes(alpha = cooc,colour = as_factor(node.community)), arrow = arrow(length = unit(4, 'mm')), 
                  end_cap = circle(5, 'mm'), width = 2, show.legend = FALSE)+
  geom_node_text(aes(label = name), colour = "#0D0D0D", size = 5, family="georgia")+
  ggtitle("Degree 1 Ego Network of Human") +
  theme(plot.title = element_textbox(face = "bold", color = "#0D0D0D", 
                                     family = "georgia", size = , hjust = 0.5),
        
        plot.background = element_rect(color = "#0D0D0D", size = 1),
        panel.background = element_rect(fill = "#F0F0F2"))


#Trimming Semantic Network to Extract Ego Network of Subject
sem_net_trimmed <- delete_edges(sem_net, E(sem_net)[cooc < 150])

#Extracting the Ego-Network of Subject
ego_network <- ego(sem_net_trimmed, node = "subject", order=1, mode="all")
ego_network <- ego_network[[1]]
induced_ego <- induced_subgraph(sem_net_trimmed, ego_network)


vcount(induced_ego)
ecount(induced_ego)

#Finding Communities in Ego Network
members <- cluster_walktrap(induced_ego)

#Assigning Communities
V(induced_ego)$community <- members$membership

#Drawing Ego Network with Communities
set.seed(358)
ggraph(induced_ego, layout="fr") +
  geom_edge_link2(aes(alpha = cooc,colour = as_factor(node.community)), arrow = arrow(length = unit(4, 'mm')), 
                  end_cap = circle(5, 'mm'), width = 2, show.legend = FALSE)+
  geom_node_text(aes(label = name), colour = "#0D0D0D", size = 5, family="georgia")+
  ggtitle("Degree 1 Ego Network of Subject") +
  theme(plot.title = element_textbox(face = "bold", color = "#0D0D0D", 
                                     family = "georgia", size = , hjust = 0.5),
        
        plot.background = element_rect(color = "#0D0D0D", size = 1),
        panel.background = element_rect(fill = "#F0F0F2"))



#Trimming Semantic Network to Extract Ego Network of World
sem_net_trimmed <- delete_edges(sem_net, E(sem_net)[cooc < 250])

#Extracting the Ego-Network of World
ego_network <- ego(sem_net_trimmed, node = "world", order=1, mode="all")
ego_network <- ego_network[[1]]
induced_ego <- induced_subgraph(sem_net_trimmed, ego_network)


vcount(induced_ego)
ecount(induced_ego)

#Finding Communities in Ego Network
members <- cluster_walktrap(induced_ego)

#Assigning Communities
V(induced_ego)$community <- members$membership

#Drawing Ego Network with Communities
set.seed(358)
ggraph(induced_ego, layout="fr") +
  geom_edge_link2(aes(alpha = cooc,colour = as_factor(node.community)), arrow = arrow(length = unit(4, 'mm')), 
                  end_cap = circle(5, 'mm'), width = 2, show.legend = FALSE)+
  geom_node_text(aes(label = name), colour = "#0D0D0D", size = 5, family="georgia")+
  ggtitle("Degree 1 Ego Network of World") +
  theme(plot.title = element_textbox(face = "bold", color = "#0D0D0D", 
                                     family = "georgia", size = , hjust = 0.5),
        
        plot.background = element_rect(color = "#0D0D0D", size = 1),
        panel.background = element_rect(fill = "#F0F0F2"))


#Trimming Semantic Network to Extract Ego Network of Voice
sem_net_trimmed <- delete_edges(sem_net, E(sem_net)[cooc < 50])

#Extracting the Ego-Network of Voice
ego_network <- ego(sem_net_trimmed, node = "voice", order=1, mode="all")
ego_network <- ego_network[[1]]
induced_ego <- induced_subgraph(sem_net_trimmed, ego_network)


vcount(induced_ego)
ecount(induced_ego)


#Finding Communities in Ego Network
members <- cluster_walktrap(induced_ego)

#Assigning Communities
V(induced_ego)$community <- members$membership

#Drawing Ego Network with Communities
set.seed(358)
ggraph(induced_ego, layout="fr") +
  geom_edge_link2(aes(alpha = cooc,colour = as_factor(node.community)), arrow = arrow(length = unit(4, 'mm')), 
                  end_cap = circle(5, 'mm'), width = 2, show.legend = FALSE)+
  geom_node_text(aes(label = name), colour = "#0D0D0D", size = 5, family="georgia")+
  ggtitle("Degree 1 Ego Network of Voice") +
  theme(plot.title = element_textbox(face = "bold", color = "#0D0D0D", 
                                     family = "georgia", size = , hjust = 0.5),
        
        plot.background = element_rect(color = "#0D0D0D", size = 1),
        panel.background = element_rect(fill = "#F0F0F2"))




#Trimming Semantic Network to Extract Ego Network of Subjectivity
sem_net_trimmed <- delete_edges(sem_net, E(sem_net)[cooc < 50])

#Extracting the Ego-Network of Subjectivity
ego_network <- ego(sem_net_trimmed, node = "subjectivity", order=1, mode="all")
ego_network <- ego_network[[1]]
induced_ego <- induced_subgraph(sem_net_trimmed, ego_network)


vcount(induced_ego)
ecount(induced_ego)


#Finding Communities in Ego Network
members <- cluster_walktrap(induced_ego)

#Assigning Communities
V(induced_ego)$community <- members$membership

#Drawing Ego Network with Communities
set.seed(358)
ggraph(induced_ego, layout="fr") +
  geom_edge_link2(aes(alpha = cooc,colour = as_factor(node.community)), arrow = arrow(length = unit(4, 'mm')), 
                  end_cap = circle(5, 'mm'), width = 2, show.legend = FALSE)+
  geom_node_text(aes(label = name), colour = "#0D0D0D", size = 5, family="georgia")+
  ggtitle("Degree 1 Ego Network of Subjectivity") +
  theme(plot.title = element_textbox(face = "bold", color = "#0D0D0D", 
                                     family = "georgia", size = , hjust = 0.5),
        
        plot.background = element_rect(color = "#0D0D0D", size = 1),
        panel.background = element_rect(fill = "#F0F0F2"))




#Trimming Semantic Network to Extract Ego Network of Way
sem_net_trimmed <- delete_edges(sem_net, E(sem_net)[cooc < 400])

#Extracting the Ego-Network of Way
ego_network <- ego(sem_net_trimmed, node = "way", order=1, mode="all")
ego_network <- ego_network[[1]]
induced_ego <- induced_subgraph(sem_net_trimmed, ego_network)


vcount(induced_ego)
ecount(induced_ego)


#Finding Communities in Ego Network
members <- cluster_walktrap(induced_ego)

#Assigning Communities
V(induced_ego)$community <- members$membership

#Drawing Ego Network with Communities
set.seed(358)
ggraph(induced_ego, layout="fr") +
  geom_edge_link2(aes(alpha = cooc,colour = as_factor(node.community)), arrow = arrow(length = unit(4, 'mm')), 
                  end_cap = circle(5, 'mm'), width = 2, show.legend = FALSE)+
  geom_node_text(aes(label = name), colour = "#0D0D0D", size = 5, family="georgia")+
  ggtitle("Degree 1 Ego Network of Way") +
  theme(plot.title = element_textbox(face = "bold", color = "#0D0D0D", 
                                     family = "georgia", size = , hjust = 0.5),
        
        plot.background = element_rect(color = "#0D0D0D", size = 1),
        panel.background = element_rect(fill = "#F0F0F2"))



#Trimming Semantic Network to Extract Ego Network of Technology
sem_net_trimmed <- delete_edges(sem_net, E(sem_net)[cooc < 400])

#Extracting the Ego-Network of Technology
ego_network <- ego(sem_net_trimmed, node = "technology", order=1, mode="all")
ego_network <- ego_network[[1]]
induced_ego <- induced_subgraph(sem_net_trimmed, ego_network)


vcount(induced_ego)
ecount(induced_ego)


#Finding Communities in Ego Network
members <- cluster_walktrap(induced_ego)

#Assigning Communities
V(induced_ego)$community <- members$membership

#Drawing Ego Network with Communities
set.seed(358)
ggraph(induced_ego, layout="fr") +
  geom_edge_link2(aes(alpha = cooc,colour = as_factor(node.community)), arrow = arrow(length = unit(4, 'mm')), 
                  end_cap = circle(5, 'mm'), width = 2, show.legend = FALSE)+
  geom_node_text(aes(label = name), colour = "#0D0D0D", size = 5, family="georgia")+
  ggtitle("Degree 1 Ego Network of Technology") +
  theme(plot.title = element_textbox(face = "bold", color = "#0D0D0D", 
                                     family = "georgia", size = , hjust = 0.5),
        
        plot.background = element_rect(color = "#0D0D0D", size = 1),
        panel.background = element_rect(fill = "#F0F0F2"))

################################################################################
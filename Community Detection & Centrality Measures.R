#######################################################
###### Social Network Analysis: Group Assignment ######
### Facebook network centrality/community detection ###
#######################################################

library(data.table)
library(igraph)

data = fread("C:/Users/vernoult/Desktop/SNA/Group Project/facebook_combined.txt")
data = as.data.frame(data)
data_graph = graph.data.frame(data)
summary(data_graph)
plot(data_graph)

#centralities
degree_cent = degree(data_graph)
close_cent = closeness(data_graph, vids = V(data_graph))
eigen_cent = centr_eigen(data_graph)
betw_cent = betweenness(data_graph, v = V(data_graph))
bon_cent = alpha_centrality(data_graph, nodes = V(data_graph), alpha = 1, loops = FALSE, tol = 1e-07, sparse = TRUE)

hist(degree_cent)
hist(close_cent)
hist(eigen_cent$vector)
hist(betw_cent)
hist(bon_cent)

summary(degree_cent)
summary(close_cent)
summary(eigen_cent$vector)
summary(betw_cent)
summary(bon_cent)

#dataframe of centrality measures
degree_cent_df = as.data.frame(degree_cent)
close_cent_df = as.data.frame(close_cent)
eigen_cent_df = as.data.frame(eigen_cent$vector)
betw_cent_df = as.data.frame(betw_cent)
bon_cent_df = as.data.frame(bon_cent)

centrality_full_data_frame <- cbind(degree_cent_df, 
                                    close_cent_df, 
                                    eigen_cent_df,
                                    betw_cent_df,
                                    bon_cent_df)

#change the names of each column
names(centrality_full_data_frame)[1:5] <- c("Degree", "Closeness", "Eigenvector", "Betwenness", "Bonacich") 
head(centrality_full_data_frame)

#making the data a graph item
data <- graph.data.frame(data)
data_und <- as.undirected(data, mode='collapse')
data_no_iso <- igraph::delete.vertices(data_und, V(data_und)[igraph::degree(data_und)==0])
summary(data_und)
summary(data_no_iso)

#community detection - walktrap
(data_comm_wt <- walktrap.community(data_no_iso, steps=200,modularity=TRUE))
plot(data_comm_wt, data_no_iso)
data_comm_dend <- as.dendrogram(data_comm_wt, use.modularity=TRUE)
plot(data_comm_dend)
modularity(data_comm_wt, data_comm_wt$membership)

#community detection - greedy
(data_comm_fg <- fastgreedy.community(data_no_iso, modularity=TRUE))
plot(data_comm_fg, data_no_iso)
data_comm_dend <- as.dendrogram(data_comm_fg, use.modularity=TRUE)
plot(data_comm_dend)
modularity(data_comm_fg, data_comm_fg$membership)

#add the community each node belongs to as a column to centrality data frame
centrality_full_data_frame <- cbind(centrality_full_data_frame, data_comm_wt$membership)
names(centrality_full_data_frame)[6] <- c("Belongs_To")



# Plot raw data
plot(data_graph, edge.arrow.mode=0, vertex.label=NA, vertex.size=3.5, 
     edge.arrow.size=0.0001, edge.arrow.width=0.0001, edge.color="black")
  
# Plot Walktrap communities
plot(data_graph, vertex.color=data_comm_wt$membership, 
     edge.arrow.mode=0, vertex.label=NA, vertex.size=4, 
     edge.arrow.size=0.0001, edge.arrow.width=0.0001, edge.color="black",
     edge.arrow.cex=0.1)

# Plot Fast & Greedy communities
plot(data_graph, vertex.color=data_comm_fg$membership, 
     edge.arrow.mode=0, vertex.label=NA, vertex.size=4, 
     edge.arrow.size=0.0001, edge.arrow.width=0.0001, edge.color="black",
     edge.arrow.cex=0.1)

c <- cbind(data_comm_fg$membership, data_comm_wt$membership)
c <- data.frame(c)
names(c) <- c("a", "b")
c$comb <- as.factor(paste0(c$a, c$b))

freq_tab <- data.frame(table(c$comb))

barplot(freq_tab$Freq, names.arg=freq_tab$Var1)


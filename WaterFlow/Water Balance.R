# igraph water balance

# Define Network
library(igraph)
g <- graph_from_literal(A-+B:C:D, C-+E:F, D-+G, E:F-+H, G-+F)

# Randomised flows
set.seed(2017)
V(g)$flows <- c(sample(30:45, 1), sample(10:15, 3),
                sample(05:10, 2), sample(05:10, 1), 
                sample(10:15, 1))

# Visualise network
visualise <- function(x) {
    plot(x, layout = layout_as_tree, 
        vertex.color = NA, 
        vertex.label.color = "black",
        vertex.frame.color="grey",
        vertex.size =32,
        vertex.label = paste(V(x)$name, V(x)$flows),
        edge.color = "black",
        edge.arrow.size=.5)
}
par(mar=rep(0,4), mfrow=c(1,1))
visualise(g)

# Water Balance
balance <- function(x){
    upstream <- which(degree(x, mode="in")==0)
    downstream <- which(degree(x, mode="out")==0)
    total.balance <-  sum(V(g)$flows[downstream]) - sum(V(g)$flows[upstream])
    paste(paste(V(g)$flows[upstream], collapse=" + "), "-", paste(V(g)$flows[downstream], collapse=" + "), "=", total.balance)
}

balance(g)

# Define Water Balance Zones
g1 <- g
par(mfrow=c(2, 3), mar=rep(0,4))
while (length(V(g1)) != 0) { # Keep chopping until all vertices have been used
        upstream <- which(degree(g1, mode="in")==0)
        for (i in upstream) {
            h <- induced_subgraph(g1, c(i, neighbors(g1, i)))
            visualise(h)
            print(balance(h))
        }
    g1 <- delete_vertices(g1, upstream)
    g1 <- delete_vertices(g1, V(g1)[degree(g1)==0])
}




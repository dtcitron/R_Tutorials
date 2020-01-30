dev.off()

library(igraph)

qnet = graph_from_adjacency_matrix(Q, weighted = TRUE, mode = "directed")
qnet.n = graph_from_adjacency_matrix(N, weighted = TRUE, mode = "directed")
qnet.c = graph_from_adjacency_matrix(C, weighted = TRUE, mode = "undirected")

# Showing the hub-periphery structure of the example landscape

# Identify the edges where the weight passes soem threshold
e.sublist <- c(1:ecount(qnet.n))[E(qnet.n)$weight > .01]
# Create a directed graph that is a subgraph of n, including only the strongest directed edges
qnet.n.sub <- subgraph.edges(qnet.n, e.sublist, delete.vertices = FALSE)
# set the weights
E(qnet.n.sub)$width <- E(qnet.n)$weight[e.sublist]*10
#
plot(qnet.n.sub, 
     vertex.color = "blue", vertex.shape = "square",
     vertex.label.color = "white",
     edge.arrow.size = .5 )

# LF-FL bipartite network, visualizing both types of nodes
# Construct full, bipartite, asymmetric adjacency matrix
A <- rbind(cbind(lzero, LF), cbind(FL, fzero))
# Can check:
# QQ <- A %*% A
# Q <- LF %*% FL
# QQ[1:29,1:29] - Q # should equal 0 everywhere!

anet = graph_from_adjacency_matrix(A, mode = "directed", weighted = TRUE)
# add type to anet
V(anet)$type = c(rep(TRUE, dim(LF)[1]), rep(FALSE, dim(FL)[1]))

V(anet)$color = c("red", "blue")[1*(V(anet)$type+1)]
V(anet)$shape = c("circle", "square")[1*(V(anet)$type+1)]
V(anet)$label = c(c(1:dim(LF)[1],c(1:dim(FL)[1])))

plot(anet, edge.arrow.size = .2, alpha = .1)
# What about generating a layout that corresponds to the x and y positions of the l and f sites?
plot(anet, layout = cbind(c(xl,xf), c(yl,yf)))
# or, we can set the (x,y) coordinates of each of the nodes
V(anet)$xpos = c(xl,xf)
V(anet)$ypos = c(yl,yf)
plot(anet, layout = cbind(V(anet)$xpos, V(anet)$ypos), 
     vertex.size = 8,
     vertex.label.color = "",
     edge.arrow.size = .25, edge.width = .1)

# Implementing community detection:
# Start with Q, C, N, where the rows of Q are normalized
# There are 5 ways that we can do this:
# 1. Walk Trap algorithm on the symmetric network C, then add in flows later with N
walk.c <- cluster_walktrap(qnet.c, weights = E(qnet.c)$weight)
dendPlot(walk.c, mode = "hclust")
plot(walk.c, qnet.n,
     edge.arrow.size = E(qnet.n)$weight*20, 
     edge.width = E(qnet.n)$weight*20
     ) 
plot(qnet.n, 
     vertex.color = walk.c$membership,
     edge.arrow.size = E(qnet.n)$weight*2, edge.width = E(qnet.n)$weight*2#,
     )
### need to figure out now how to draw coarse-grained picture of these communities
# number of clusters
m <- membership(walk.c)
k <- length(unique(m))
coarse.grain <- array(0, dim = c(k,k))
for (i in seq(1,length(E(qnet.n)))) {
  xy <- ends(qnet.n, E(qnet.n)[i]);
  w <- E(qnet.n)$weight[i];
  coarse.grain[m[xy[1]],m[xy[2]]] = coarse.grain[m[xy[1]],m[xy[2]]] + w}
cg.net <- graph_from_adjacency_matrix(coarse.grain, weighted = TRUE, mode = "directed")
plot(cg.net, edge.width = (E(cg.net)$weight)^.2,# edge.arrow.size = (E(cg.net)$weight)^.2/2,
     vertex.color = c(1:k), vertex.size = 3*matrix(table(membership(walk.c))))
# 2. InfoMap algorithm on the full directed network Q
info.q <- cluster_infomap(qnet, e.weights = edge_attr(qnet)$weight, modularity = FALSE)
# 3. InfoMap algorithm on the symmetric network C, then add flows later with N
info.c <- cluster_infomap(qnet.c, e.weights = edge_attr(qnet.c)$weight, modularity = FALSE)
# 4. Betweenness algorithm on the full directed network Q
girv.q <- cluster_edge_betweenness(qnet, weights = E(qnet)$weight, directed = TRUE, edge.betweenness = TRUE, modularity = FALSE)
# 5. Betweenness algorithm on the symmetric netowrk C, then add flows later with N

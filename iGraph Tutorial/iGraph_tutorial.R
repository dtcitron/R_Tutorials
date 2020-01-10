# http://igraph.org/r/
library(igraph)
# tutorial here: http://kateto.net/networks-r-igraph

# Generate an undirected graph with three edges.  Numbers are interpreted as vertex IDs:
g1 <- graph(edges = c(1,2,2,3,3,1), n = 3, directed = FALSE)
# The graph object is of a class igraph:
class(g1)
summary(g1)
# plot the graph
plot(g1)
# can also format the edge list in a less confusing way:
e = c(1,2,2,3,3,1); dim(e) <- c(2,3)
plot(graph(edges = e, n = 3, directed = FALSE))
# can also add additional vertices that are not included in the edge list:
g2 = graph(edges = e, n = 10, directed = TRUE)
plot(g2)
# Name the nodes:
g3 <- graph( c("John", "Jim", "Jim", "Jill", "Jill", "John"))
plot(g3)
# Summarizing g3 will show that the nodes now have an additional attribute: names
g3
# Another example:
g4 <- graph( c("John", "Jim", "Jim", "Jack", "Jack", "Jim", "John", "John"), 
             isolates=c("Jesse", "Janis", "Jennifer", "Justin") ) 
# with fancy labeling:
plot(g4,
     edge.arrow.size = .5, vertex.color = "gold", vertex.size = 15,
     vertex.frame.color = 'red', vertex.label.color = "black",
     vertex.label.cex =1.5, vetex.label.dist = 2, edge.curved = .25
     )

# Edge, vertex, network attributes:
E(g4)
V(g4)
# adjacency matrix:
g4[]
# neighbors of node "John"
g4["John"]
# Same as finding neighbors of node 1:
g4[1]
# Can also pull out rows/columns this way:
g4[,1]


# Each node is a list, and can be assigned attributes
V(g4)$gender <- c("male", "male", "male", "male", "female", "female", "male")
E(g4)$type <- "email"
E(g4)$weight <- 10
# Retrieving those attributes:
V(g4)$gender
V(g4)["John"]$gender
# summarizing all vertex attributes
vertex.attributes(g4)
# summarizing all node attributes
edge.attributes(g4)
# Can also assign attributes this way:
g4 <- set_graph_attr(g4, "name", "Email network")
graph_attr(g4)
graph_attr(g4)$name; graph_attr(g4)["name"]
# Delete an attribute
g4 <- set_graph_attr(g4, "something", "something")
g4 <- delete_graph_attr(g4, "something")
graph_attr(g4)
# Can color the graph according to attributes:
plot( g4, edge.arrow.size = .5, vertex.label.color = "black", 
      vertex.color = c("red", "blue")[1 + (V(g4)$gender == "male")])



# Graph Generation:
# empty graph
eg <- make_empty_graph(40)
plot(eg)
# full graph
fg <- make_full_graph(15)
plot(fg)
# ER graph
er <- sample_gnm(n = 100, m = 40)
plot(er, vertex.label = NA, vertex.size = 5)
# Watts-Strogatz model
ws <- sample_smallworld(dim = 2, size = 10, nei = 1, p = 0.1)
plot(ws, vertex.label = NA, vertex.size = 5, layout = layout_in_circle)
# Barabasi-Albert preferential attachment model
ba <- sample_pa(n = 100, power = 1, m = 1, directed = FALSE)
plot(ba, vertex.label = NA, vertex.size = 5)


# Generating networks from data
nodes <- read.csv('Documents/R Tutorials/netscix2016/Dataset1-Media-Example-NODES.csv', header = T, as.is=T)
edges <- read.csv('Documents/R Tutorials/netscix2016/Dataset1-Media-Example-EDGES.csv', header = T, as.is=T)
# Examining the data, using the header:
head(nodes)
# other ways of summarizing the data
summary(nodes)
nrow(nodes)
length(unique(nodes$id))
# count number of edges, and number of unique edges
nrow(edges); nrow(unique(edges[,c("from", "to")]))
# there are more edges than unique edges: edges are repeated
# let's collapse all edges of the same type
# This function takes the edge weights (column 3 of edges), aggregates together all cases (from-to-type), 
# and sums the weights accordingly
links <- aggregate(edges[,3], edges[,-3], sum)
# we are left with the 49 unique (from-to-type) edges, with summed weights
links <- links[order(links$from, links$to),]

# Another data set, this time a bipartite graph showing links between news sources and consumers:
nodes2 <- read.csv('Documents/R Tutorials/netscix2016/Dataset2-Media-User-Example-NODES.csv', header=T, as.is=T)
links2 <- read.csv('Documents/R Tutorials/netscix2016/Dataset2-Media-User-Example-EDGES.csv', header=T, row.names=1)
head(nodes2)
head(links2)

# Now, let's take the data frames and transform them into igraph objects:
net <- graph_from_data_frame(d = links, vertices = nodes, directed = T)
class(net)
net
vertex_attr(net)
edge_attr(net)
# etc
# We can also show the adjacency matrix: 
as_adjacency_matrix(net)
# We can also show the adjacency matrix, adjusting edges according to weight (x)
as_adjacency_matrix(net, attr = "x")
# Now, plot the network:
plot(net, edge.arrow.size = .4, vertex.label = NA)
# Let's clean this up by removing self-loops:
net <- simplify(net, remove.multiple = F, remove.loops = T)
plot(net, edge.arrow.size = .4, vertex.label = NA)

# For the second graph, the data are all in a matrix form:
net2 <- graph_from_incidence_matrix(links2)
table(V(net2)$type)

# Sidebar: what is bipartite projection doing?
# A bipartite graph has edges only between unlike node types, so we have a block-off-diagonal adj matrix
# Suppose we want to see how nodes of like type interact with one another (as mediated through interactions
# with nodes of unlike type)
#
# In the world of iGraph, a bipartite graph's nodes are labeled with types as T or F
# So, we can find all of the True-True edges by integrating out the F-type nodes, and vice-versa for F-F edges
g <- make_full_bipartite_graph(10,5) # generate bipartite graph
m <- as.matrix(as_adjacency_matrix(g)) # generate adjacency matrix
bproj <-  (m %*% t(m) > 0) * 1 # multiply matrix by itself, to find block-diagonal form
# This is the submatrix of all T-T node interactions:
proj1 <- graph_from_adjacency_matrix(bproj[c(get.vertex.attribute(g)$type),c(get.vertex.attribute(g)$type)], mode = 'undirected')
# This is the submatrix of all F-F node interactions
proj2 <- graph_from_adjacency_matrix(bproj[!c(get.vertex.attribute(g)$type),!c(get.vertex.attribute(g)$type)], mode = 'undirected')
# And plot the graphs accordingly:
proj1 <- simplify(proj1)
proj2 <- simplify(proj2)
plot(proj1 %du% proj2)
# Compare this now to the use of bipartite.projection()
proj <- bipartite.projection(g)
dplot(proj[1] %du% proj[2])

# Returning to the bipartite user-source media data, we can show network of co-memberships:
net2 <- graph_from_incidence_matrix(links2)
net2.bp <- bipartite.projection(net2)
plot(net2.bp$proj1, vertex.label.color='black', vertex.label.dist=1,
     vertex.size =7, vertex.label=nodes2$media[!is.na(nodes2$media.type)])
plot(net2.bp$proj2, vertex.label.color='black', vertex.label.dist=1,
     vertex.size =7, vertex.label=nodes2$media[is.na(nodes2$media.type)])



# Plotting networks with iGraph:
plot(net, edge.arrow.size = .4, edge.curved = .5)
# Set the edge color, node color, and replace the vertex labels with names
plot(net, edge.arrow.size = .4, edge.curved = 0, edge.color = "gray",
     vertex.color = "orange", vertex.frame.color = "black",
     vertex.label.color = "black",
     vertex.label = V(net)$media,
     vertex.label.cex = 0.7)
# Another way to specify plot attributes is to add them to the igraph object itself:
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type] # ie, color different media types different colors:
# Set node size based on audience size:
V(net)$size <- V(net)$audience.size*.7
# Setting node labels to NA will remove them
V(net)$label <- NA
# Set edge width based on weight
E(net)$width <- E(net)$weight/6.
# Specify edge arrow size and color:
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"
E(net)$width <-  1 + E(net)$weight/12

plot(net)
# can also explicitly override when plotting:
plot(net, vertex.label = V(net)$media)
# or
plot(net, edge.color="orange", vertex.color="gray50") 
# Adding in a legend
plot(net)
legend(x=-1.5, y=-1.1, c("Newspaper","Television", "Online News"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)
# Remove nodes - only show labels:
plot(net, vertex.shape = "none", vertex.label = V(net)$media)
# Color edges of the graph based on node source color using ends()
# ends() returns a list of pairs of connected nodes, preserves direction
edge.start <- ends(net, es = E(net), names = F)
V(net)$color <- colrs[V(net)$media.type] # recall: we colored the network based on media type
edge.colrs <- V(net)$color[edge.start]
plot(net, edge.color = edge.colrs, edge.curved = 0, arrow.size = 10)

# Playing with Network Layouts:
net.bg <- sample_pa(80)
V(net.bg)$size <- 8
V(net.bg)$frame.color <- "white"
V(net.bg)$color <- "orange"
V(net.bg)$label <- NA
E(net.bg)$arrow.mode <- FALSE
plot(net.bg)
# Set to a random layout in the plot function
plot(net.bg, layout=layout_randomly)
# Can also calculate the vertex coordinates ahead of time:
l <- layout_in_circle(net.bg)
plot(net.bg, layout=l)
# fructerman-reingold, a common force-directed layout algorithm
l <- layout_with_fr(net.bg)
plot(net.bg, layout=l)
# The layout is not deterministic, but we can set it with seed:
set.seed(1)
l <- layout_with_fr(net.bg)
plot(net.bg, layout = l)
# By default, the layout is scaled to [-1,1] in both x and y.  We can change this setting rescale=FALSE:
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
par(mfrow=c(2,2), mar = c(0,0,0,0))
plot(net.bg, rescale=F, layout = l*.4)
plot(net.bg, rescale=F, layout = l*.8)
plot(net.bg, rescale=F, layout = l*1.2)
plot(net.bg, rescale=F, layout = l*1.6)
# Another popular force-directed layout algorithm is Kamada Kawai
l <- layout_with_kk(net.bg)
plot(net.bg, layout = l)
# good for large, connected graphs
plot(net.bg, layout = layout_with_lgl)

# ALL available built-in layouts in igraph:
layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1]
# removing the irrelevant layouts:
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]
#setup:
par(mfrow=c(3,5), mar=c(1,1,1,1))
for (layout in layouts) {
  print(layout)
  l <- do.call(layout, list(net)) 
  plot(net, edge.arrow.mode=0, layout=l, main=layout) }


# Other network representations:
# A heatmap of the network adjacency matrix:
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T)
# look! we can set the weighting of the nonzero entries in the adjacency matrix to values
# according to different attributes!
netm <- get.adjacency(net, attr='x', sparse = F)
palf <- colorRampPalette(c("gold","red"))
heatmap(netm[,17:1], Rowv = NA, Colv = NA, col = palf(100), scale = "none", margins = c(10,10))
E(net)$weight <- 10
netm.w <- get.adjacency(net, attr = 'weight', sparse = F)
heatmap(netm.w[,17:1], Rowv = NA, Colv = NA, col = palf(100), scale = "none", margins = c(10,10))
# now, we can set the names of the edges too!
colnames(netm) <- V(net)$media
rownames(netm) <- V(net)$media
heatmap(netm[,17:1], Rowv = NA, Colv = NA, col = palf(100), scale = "none", margins = c(10,10))

# Plotting bipartite networks!
dev.off() # clear "device" ie graphics
# Let's set all sources as blue squares, and all users as orange circles
V(net2)$color <- c("steel blue", "orange")[V(net2)$type + 1]
V(net2)$shape <- c("square", "circle")[V(net2)$type + 1]
V(net2)$size <- c(20, 10)[V(net2)$type + 1]
# remove user labels
V(net2)$label <- ""
V(net2)$label[V(net2)$type == F] = nodes2$media[V(net2)$type == F]
# set label size and font
V(net2)$label.cex=1; V(net2)$label.font = 4
plot(net2, vertex.label.color = 'white')
# Using text as nodes:
plot(net2, vertex.shape = "none", vertex.label = nodes2$media,
     vertex.label.color = V(net2)$color, vertex.label.font = 2.5, vertex.label.cex = 0.6)
# There is also another special layout for bipartite networks, but it doesn't always work great:
plot(net2, layout = layout_as_bipartite)


# Network and node descriptives
# Edge density
edge_density(net, loops = F) # note that this counts directed edges, not undirected edges, so the number of possible edges is doubled
ecount(net)/vcount(net)/(vcount(net)-1)
# Reciprocity - how many directed edges are reciprocated
reciprocity(net)
dyad_census(net)$mut*2/(ecount(net)-1)
net.u <- as.undirected(net, mode = "collapse") # reciprocity is complete in an undirected graph
reciprocity(net.u)
# Transitivity
transitivity(net, type = "global") # ratio of triangles to connected triples (direction is ignored)
transitivity(net, type = "local") # ratio of triangles to connected triples each vertex belongs to
# Diameter - longest geodesic distance
diameter(net, directed =F, weights = NA)
# Distances and paths:
mean_distance(net, directed = F) # mean geodesic distance, on an undirected graph
distances(net, weights = NA) # calculate matrix of all distances between nodes
# calculate distances from the NYT to the rest of the graph:
dist.from.NYT <- distances(net, v = V(net)[media == 'NY Times'], to = V(net), weights = NA)
# visualize all nodes in terms of their distance from the NYT:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(dist.from.NYT) + 1)
col <- col[dist.from.NYT + 1]
plot(net, vertex.color = col, vertex.label = dist.from.NYT)
# Identify the shortest path between two nodes: MSNBC and the NY Post:
news.path <- shortest_paths(net,
                            from = V(net)[media=="MSNBC"],
                            to  = V(net)[media=="New York Post"],
                            output = "both")
ecol <- rep("gray80", ecount(net)) # color each edge gray
ecol[unlist(news.path$epath)] = "red" # color each edge in news.path gray
vcol <- rep("gray40", vcount(net)) # color each vertex
vcol[unlist(news.path$vpath)] = "gold" # color each vertex in news.path gray
plot(net, vertex.color = vcol, edge.color = ecol, edge.arrow.mode = 0)
# Identify the edges going into or out of a single vertex: WSJ
inc.edges = incident(net, V(net)[media == "Wall Street Journal"], mode = "all")
ecol <- rep("gray80", ecount(net)) # color each edge gray
ecol[unlist(inc.edges)] <- rep("orange", length(inc.edges)) # highlight edges connecting to WSJ node
vcol <- rep("gray40", vcount(net)) # color each node gray
vcol[unlist(V(net)[media == "Wall Street Journal"])] <- "gold"
plot(net, vertex.color = vcol, edge.color = ecol, edge.arrow.mode = 0)
# Identify the neighbors of the WSJ
neigh.nodes <- neighbors(net, V(net)[media == "Wall Street Journal"], mode = "out")
vcol[neigh.nodes] <- "orange"
plot(net, vertex.color = vcol, edge.color = ecol, edge.arrow.mode = 0)
# There are some special operators for indexing edge sequences : %-%, %<-%, %->%
# We can select all edges pointing from newspapers to online sources:
E(net)[V(net)[type.label == "Newspaper"] %->% V(net)[type.label == "Online"]]

# Node degrees
degree(net, mode = "in") # in-degree sequence
degree(net, mode = "out") # out-degree sequence
degree(net, mode = "all") # sum in and out degrees
# visualize
deg <- degree(net, mode = "all")
plot(net, vertex.size = deg*3)
# histogram of degree distribution
hist(deg, breaks=1:vcount(net)-1, main="Histogram of node degree")


# Subgroups and Communities
# Use an undirected network - sum the edge weights
net.sym <- as.undirected(net, mode = "collapse", edge.attr.comb = list(weight = "sum", "ignore"))
# Find cliques
cliques(net.sym)
# Print the list of clique sizes
sort(sapply(cliques(net.sym), length), decreasing = TRUE)
# Highlight the largest clique
vcol <- rep("gray80", vcount(net.sym))
vcol[unlist(largest_cliques(net.sym))] <- "gold"
plot(as.undirected(net.sym), vertex.label = V(net.sym)$name, vertex.color = vcol)

# Community detection
# Newman-Girvan - based on edge betweenness
ceb <- cluster_edge_betweenness(net.sym)
dendPlot(ceb, mode = "hclust")
plot(ceb, net)
# Fast/greedy modularity maximization
cfg <- cluster_fast_greedy(as.undirected(net))
plot(cfg, as.undirected(net))
# Can also plot communities without relying on the built-in plot
V(net)$community <- cfg$membership
colrs <- adjustcolor(c("gray50", "tomato", "gold", "blue"), alpha = .6);
plot(net, vertex.color = colrs[V(net)$community])

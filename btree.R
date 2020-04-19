install.packages("igraph")
library(igraph)
G <-  graph.extended.chordal.ring(13, matrix(c(2,4,6), nr=1))
L <- layout.fruchterman.reingold(G)

library(igraph)
G <- graph.tree(n=13,children=2)

# let's print it using a tree-specific layout 
# (N.B. you must specify the root node)
co <- layout.reingold.tilford(G, params=list(root=1)) 
plot(G, layout=co)


library(igraph)
G <- graph.tree(n=13,children=2)

#add names to vertex (just assign a upper-case letter to each)
V(G)$name <- LETTERS[1:length(V(G))]

# plot (1)
lay <- layout.reingold.tilford(G, params=list(root='A')) 
plot(G, layout=lay, vertex.size=25)

# add a vertex 'O', then a new edge 'G' --> 'O'
G <- G + vertices('O')
G <- G + edge('G', 'O')

# plot again (2)
lay <- layout.reingold.tilford(G, params=list(root='A')) 
plot(G, layout=lay, vertex.size=25)


install.packages("ctv")
library(ctv)

install.views("Phylogenetics")
update.views("Phylogenetics")

library(ape)
#simulate phylogeny
tree <- rtree(n = 20)
plot(tree, edge.width = 2)

#a tree with 5 tips & no edge lengths
tree <- read.tree(text = "(((A,B),(C,D)),E);")
plot(tree, type = "cladogram", edge.width = 2)

plot(tree, edge.width = 2, label.offset = 0.1, type = "cladogram")
nodelabels()
tiplabels()

## (I'm going to first set the seed for repeatability)
set.seed(1)
## simulate a birth-death tree using phytools
tree <- pbtree(b = 1, d = 0.2, n = 40)
## stopping criterion is 40 extant species, in this case
plot.phylo(tree, setEnv = TRUE)
## ok, now extract the clade descended from node #62
tt62 <- extract.clade(tree, 62)
plot.phylo(tt62) 
## now drop 10 tips from the tree (I'm going to pick them at random)
dtips <- sample(tree$tip.label, 10)
dt <- drop.tip(tree, dtips)
plot.phylo(dt)
## we could also, say, drop all tips that go extinct before the present
## this is a fun way, but not the only way to do this:
et <- fancyTree(tree, type = "droptip", tip = getExtinct(tree), cex = 0.7)
print(et)

t1 <- read.tree(text = "((A,B,C),D);")
plot(t1, type = "cladogram")
## check if binary
is.binary.tree(t1)
## randomly resolve polytomies
t2 <- multi2di(t1)
plot(t2, type = "cladogram")
is.binary.tree(t2)
## rotating nodes
plot.phylo(tree, node.numbers = T)
## first, rotate about node #50
rt.50 <- rotate(tree, 50)
plotTree(rt.50)
## now rotate all nodes
rt.all <- rotateNodes(tree, "all")
plotTree(rt.all)
## ok, now let's re-root the tree at node #67
rr.67 <- root(tree, node = 67)
plotTree(rr.67)
## this creates a trifurcation at the root we could instead re-root at
## along an edge
rr.62 <- reroot(tree, 62, position = 0.5 * tree$edge.length[which(tree$edge[, 2] == 62)])
plotTree(rr.62)

#Comparing trees
## check if tree & rt.all are equal
all.equal(tree, rt.all)
## check if tree & rr.62 are equal
all.equal(tree, rr.62)
## check if unrooted tree & rr.62 are equal
all.equal(unroot(tree), unroot(rr.62))

#Multiple trees
## simulate 10 pure-birth trees
trees <- pbtree(n = 6, nsim = 10, scale = 1)
print(trees)
## round the edge lengths of the tree to 3 digits
trees <- roundBranches(trees, 1)
## write to file
write.tree(trees, file = "example.trees")
cat(readLines("example.trees"), sep = "\n")

# Creates a domain network based on text id's -----------------------------

dm <- read_csv('data/IRR-domains.csv')
dm <- dm[complete.cases(dm),]

d_edges <- data.frame()
for(i in unique(dm$textid)){
  tmp <- dm[dm$textid==i,]
  if(nrow(tmp) >1){
    m <- t(combn(nrow(tmp),2))
    d_edges <- rbind(
      d_edges,
      data.frame(matrix(as.character(tmp$domain)[m], ncol=2))
    )
  }
}

g <- graph_from_edgelist(as.matrix(d_edges))
E(g)$weight <- 1
V(g)$freq <- table(dm$domain)[names(V(g))]
g <- igraph::simplify(g, edge.attr.comb=list(weight='sum'))


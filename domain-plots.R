source('domain-network.R')    # creates the domain network

g2 <- induced_subgraph(g, vids=V(g)[V(g)$freq>=10])  # filters >=10

# sampled columns
dm_cols <- c(
  'medicine',
  'botany',
  'psychology',
  'injury',
  'zoology',
  'childbirth',
  'social',
  'meteorology',
  'history',
  'pregnancy/infants',
  'genealogy',
  'matchmaking',
  'geography',
  'death',
  'anatomy'
)

V(g2)$sampled <- V(g2)$name %in% dm_cols

full_domain_plot <- 
  ggraph(g2, 'nicely') +
  geom_edge_link0(aes(edge_width=weight/max(E(g)$weight)), 
                  edge_colour="lightgray", alpha=0.4) +
  geom_node_point(aes(size=freq, colour=sampled), alpha=0.6) +
  scale_colour_manual(values=viridis::magma(11)[c(8,4)]) +
  geom_node_text(aes(label=name), repel=TRUE) +
  scale_edge_width(range = c(0.05,4))+
  theme_graph(base_family='Helvetica') +
  labs(x='', y='', size='# obs.', edge_width='weights', colour='search included')


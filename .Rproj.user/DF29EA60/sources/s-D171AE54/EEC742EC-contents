all_vars <- c(hsm_vars, mpm_vars, anti_hsm, anti_mpm, ctm_vars, psm_vars, scm_vars, anti_scm)
m <- as.matrix(dist(t(dh), method='binary'))
gm <- graph_from_adjacency_matrix(m, mode = 'undirected', weighted = T, diag = F)
gm <- igraph::mst(gm, algorithm = 'prim')
V(gm)$support <- colSums(dh)

mod_spec_vars <- var_dict2[names(table(all_vars)[table(all_vars)==1])]
V(gm)$specific <- names(V(gm)) %in% mod_spec_vars

Graph_evidence <- 
  ggraph(gm, 'stress') + 
  geom_node_point(aes(size=support, colour=specific), alpha=0.5) +  
  geom_edge_link(alpha=0.5) +
  geom_node_text(aes(label=name), repel=TRUE, size=3) +
  theme_graph(base_size=12) +
  scale_colour_manual(values=viridis::magma(11)[c(8,4,1)]) +
  labs(colour='Model specific', size='Evidence') +
  annotate('text', x=6.05, y=-2.4, label='Shamans & \nlower class', size=4, fontface=2) +
  annotate('text', x=6.05, y=1.35, label='Knowledge \nrestrictions', size=4, fontface=3) +
  annotate('text', x=4.75, y=4, label='Market for \nspecialists', size=4, fontface=2) +
  annotate('text', x=-0.7, y=3.65, label='Efficacious services', size=4, fontface=3) +
  annotate('text', x=-7.25, y=-0.05, label='Access to \nmates', size=4, fontface=2) +
  annotate('text', x=-3.55, y=-2, label='Mate & kin\n provisioning', size=4, fontface=2) +
  annotate('text', x=3.4, y=-4.25, label='Prestige & \ninformation goods', size=4, fontface=2)

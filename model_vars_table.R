mod_vard <- df_evidence[df_evidence$vars!='Model score',]
mod_vard$vars <- as.character(mod_vard$vars)
mod_vard$mod_id <- as.character(mod_vard$mod_id)
# mod_vard$mod_id <- factor(mod_vard$mod_id, levels=sort(unique(mod_vard$mod_id)))
# mod_vard$mod_id <- factor(mod_vard$mod_id, levels=rev(levels(mod_vard$mod_id)))
# mod_vard$vars <- factor(mod_vard$vars, levels=rev(levels(mod_vard$vars)))

mod_vard$mod_id <- factor(mod_vard$mod_id, levels=c(
  'Collaborative\ncognition\nmodel',
  'Anti-collaborative\ncognition\nmodel',
  'Cultural\ntransmission\nmodel',
  'Proprietary\nknowledge\nmodel',
  'Honest\nsignaling\nmodel',
  'Anti-honest\nsignaling\nmodel',
  'Mate\nprovisioning\nmodel',
  'Anti-mate\nprovisioning\nmodel'
))

mod_vard <- mod_vard %>% 
  arrange(vars)

mod_vard$var_present <- 1
mod_vard$unique_vars <- as.numeric(mod_vard$unique_vars)

mod_vard2 <- mod_vard %>% 
  dplyr::select(vars, mod_id, unique_vars) %>% 
  unique() %>% 
  pivot_wider(names_from=mod_id, values_from=unique_vars, values_fill=-1)

model_vars_table <- 
  hagenheat3(mod_vard2, seriation_method='PCA_angle') +
  guides(fill=FALSE) +
    scale_fill_gradient2(low='white', mid='#F76F5CFF', high='#641A80FF')

# model_vars_table <- 
#   ggplot(mod_vard, aes(x=mod_id, y=vars)) +
#   geom_tile() +
#   theme_bw() +
#   labs(x='', y='') +
#   # coord_fixed(ratio=0.4) +
#   scale_x_discrete(position='top')

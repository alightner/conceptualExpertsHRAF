# male_female_text <- sciExpertiseHRAF::text_data$paragraph[sciExpertiseHRAF::text_data$textid %in% df$textid[df$male==1 & df$female==1 & df$sex_restriction==1]]
# View(male_female_text)
# View(dh[row.names(dh) %in% df$textid[df$sex_restriction==1],])
# rm(list=ls())
set.seed(2020)
source('packages.R')
source('functions.R')
source('mod-vars.R')
load('data/var_dict')
load('data/texrec_length')
load('data/cite_data')
df <- read_csv('data/IRR-final-dataset.csv')
culture_df <- read_csv('data/culture_df.csv')
source('world-map.R')      # This runs figure 1
source('domain-plots.R')   # This runs figure 2

# Lasso regression with sex as outcomes -----------------------------------

set.seed(2021)
col_cutoff <- 26

d0 <- df[sapply(df, is.numeric)]
d0$culture <- df$culture_id
d0$author <- df$author_id
d0$male <- NULL
d0$female <- NULL
d0$male <- df$male
d0$female <- df$female

y <- d0$male
x <- d0[sapply(d0, is.numeric)] %>%
  dplyr::select(-c(male, female)) %>%
  dplyr::select(any_of(unique(modlist2$vars))) %>%
  as.matrix()
x <- x[,colSums(x)>=col_cutoff]
male_m <- cv.glmnet(x=x, y=y, family='binomial', alpha=0, relax=TRUE, trace=TRUE, standardize=FALSE)
male_coefs <- coef(male_m, s=male_m$lambda.1se)
male_coefs <- male_coefs[-which(rownames(male_coefs)=='(Intercept)'),]
names(male_coefs) <- var_dict2[names(male_coefs)]
beta_hat <- coef(male_m, x=x, y=y, s=male_m$lambda.1se/nrow(x), exact=TRUE)
out <- fixedLassoInf(x, y, beta_hat, male_m$lambda.1se, family='binomial')
se_male <- data.frame(vars=names(out$vars), out$ci)
colnames(se_male) <- c('name', 'lowerCI', 'upperCI')
se_male$name <- var_dict2[se_male$name]

y <- d0$female
x <- d0[sapply(d0, is.numeric)] %>%
  dplyr::select(-c(male, female)) %>%
  dplyr::select(any_of(unique(modlist2$vars))) %>%
  as.matrix()
x <- x[,colSums(x)>=col_cutoff]
female_m <- cv.glmnet(x=x, y=y, family='binomial', alpha=0, relax=TRUE, trace=TRUE, standardize=FALSE)
female_coefs <- coef(female_m, s=female_m$lambda.1se)
female_coefs <- female_coefs[-which(rownames(female_coefs)=='(Intercept)'),]
names(female_coefs) <- var_dict2[names(female_coefs)]
beta_hat <- coef(female_m, x=x, y=y, s=female_m$lambda.1se/nrow(x), exact=TRUE)
out <- fixedLassoInf(x, y, beta_hat, female_m$lambda.1se, family='binomial')
se_female <- data.frame(vars=names(out$vars), out$ci)
colnames(se_female) <- c('name', 'lowerCI', 'upperCI')
se_female$name <- var_dict2[se_female$name]

se_sdf <- bind_rows(list(male=se_male, female=se_female), .id='sex')
lsdf <- 
  bind_rows(list(male=male_coefs, female=female_coefs), .id='sex') %>% 
  pivot_longer(-sex) %>% 
  left_join(se_sdf, by=c('sex','name')) %>% 
  dplyr::filter(value!=0)

male_df <- lsdf[lsdf$sex=='male',]
female_df <- lsdf[lsdf$sex=='female',]

male_df <- male_df[abs(male_df$upperCI-male_df$lowerCI)<=5,]
female_df <- female_df[abs(female_df$upperCI-female_df$lowerCI)<=5,]

male_lassoPlot <- ggplot(male_df, aes(x=exp(value), reorder(name, value))) +
  geom_point(size=2.25) +
  geom_errorbarh(aes(xmin=exp(lowerCI), xmax=exp(upperCI)), height=0.1, lwd=0.75, alpha=1) +
  geom_vline(xintercept=1, alpha=0.45) +
  scale_y_reordered() +
  scale_x_log10() +
  theme_bw() +
  labs(x='\nOdds ratio', y='') +
  ggtitle('Male experts')

female_lassoPlot <- ggplot(female_df, aes(x=exp(value), reorder(name, value))) +
  geom_point(size=2.25) +
  geom_errorbarh(aes(xmin=exp(lowerCI), xmax=exp(upperCI)), height=0.1, lwd=0.75, alpha=1) +
  geom_vline(xintercept=1, alpha=0.45) +
  scale_y_reordered() +
  scale_x_log10() +
  theme_bw() +
  labs(x='\nOdds ratio', y='') +
  ggtitle('Female experts')

# ggplot(lsdf, aes(x=exp(value), y=reorder_within(name, value, sex))) +
#   geom_point(size=2.25) +
#   geom_errorbarh(aes(xmin=exp(lowerCI), xmax=exp(upperCI)), height=0.1, lwd=0.75, alpha=1) +
#   geom_vline(xintercept=1, alpha=0.45) +
#   facet_wrap(~sex, scales='free') +
#   scale_y_reordered() +
#   scale_x_log10() +
#   theme_bw() +
#   labs(x='\nOdds ratio', y='')

# Fixed effects plot ------------------------------------------------------

# source('sex-fixed-effects.R')
# fe_sexdf$model_specific <- ifelse(fe_sexdf$unique_vars, 'specific', 'generic')
# fe_sexdf$mod_id <- str_replace_all(fe_sexdf$mod_id, pattern=' ', replacement='\n')
# 
# fixed_effects_sex_plot <- 
#   ggplot(unique(fe_sexdf), aes(x=effect, y=reorder(vars, effect), colour=model_specific)) +
#   facet_grid(mod_id~sex, scales='free_y', space='free') +
#   geom_point() +
#   geom_errorbarh(aes(xmin=lowerCI, xmax=upperCI), height=0, lwd=1.75, alpha=0.4) +
#   theme_bw(base_size=10) +
#   scale_colour_manual(values=viridis::magma(11)[c(8,4)]) +
#   geom_vline(xintercept=0, alpha=0.25) +
#   scale_shape_manual(values=c(1,16), guide=FALSE) +
#   labs(x='\nLog odds', y='', colour='Variable type') +
#   theme(strip.text.y=element_text(angle=0, hjust=0))

# Clustering analysis -----------------------------------------------------

df2 <- cbind(
  data.frame(textid=df$textid, culture_id=df$culture_id, author_id=df$author_id,
             stringsAsFactors=FALSE),
  df[colnames(df) %in% c(hsm_vars, mpm_vars, anti_hsm, anti_mpm, 
                         ctm_vars, psm_vars, scm_vars, anti_scm)]
)
dh <- as.data.frame(df2[sapply(df2, is.numeric)])
row.names(dh) <- df2$textid
dh <- dh[rowSums(dh)>0,]
colnames(dh) <- var_dict2[colnames(dh)]

# Male patterns in heatmap?
male_obs <- df %>% 
  dplyr::select(textid, male) %>% 
  right_join(data.frame(textid=row.names(dh), stringsAsFactors=FALSE), by='textid')

heat_male_obs <- hagenheat2(t(dh), dist='binary', seriation_method='PCA_angle', ann_col=male_obs) +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  scale_colour_continuous()


# Female patterns in heatmap?
female_obs <- df %>% 
  dplyr::select(textid, female) %>% 
  right_join(data.frame(textid=row.names(dh), stringsAsFactors=FALSE), by='textid')

heat_female_obs <- hagenheat2(t(dh), dist='binary', seriation_method='PCA_angle', ann_col=female_obs) +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  scale_colour_continuous()


# Minimum spanning tree vars ----------------------------------------------

all_vars <- c(hsm_vars, mpm_vars, anti_hsm, anti_mpm, 
              ctm_vars, psm_vars, scm_vars, anti_scm)

male_dh <- dh[row.names(dh) %in% df$textid[df$male==1],]
female_dh <- dh[row.names(dh) %in% df$textid[df$female==1],]

# MST for males
m <- as.matrix(dist(t(male_dh), method='binary'))
gm <- graph_from_adjacency_matrix(m, mode = 'undirected', weighted = T, diag = F)
gm <- igraph::mst(gm, algorithm = 'prim')
V(gm)$support <- colSums(male_dh)
mod_spec_vars <- var_dict2[names(table(all_vars)[table(all_vars)==1])]
V(gm)$specific <- names(V(gm)) %in% mod_spec_vars

Graph_evidence_males <- 
  ggraph(gm, 'stress') + 
  geom_node_point(aes(size=support, colour=specific), alpha=0.5) +  
  geom_edge_link(alpha=0.5) +
  geom_node_text(aes(label=name), repel=TRUE, size=3) +
  theme_graph(base_size=12) +
  scale_colour_manual(values=viridis::magma(11)[c(8,4,1)]) +
  labs(colour='Model specific', size='Evidence') +
  ggtitle('Males')


# MST for females
m <- as.matrix(dist(t(female_dh), method='binary'))
gm <- graph_from_adjacency_matrix(m, mode = 'undirected', weighted = T, diag = F)
gm <- igraph::mst(gm, algorithm = 'prim')
V(gm)$support <- colSums(female_dh)
mod_spec_vars <- var_dict2[names(table(all_vars)[table(all_vars)==1])]
V(gm)$specific <- names(V(gm)) %in% mod_spec_vars

Graph_evidence_females <- 
  ggraph(gm, 'stress') + 
  geom_node_point(aes(size=support, colour=specific), alpha=0.5) +  
  geom_edge_link(alpha=0.5) +
  geom_node_text(aes(label=name), repel=TRUE, size=3) +
  theme_graph(base_size=12) +
  # theme(legend.position = 'none') +
  scale_colour_manual(values=viridis::magma(11)[c(8,4,1)]) +
  labs(colour='Model specific', size='Evidence') +
  ggtitle('Females')


# Heatmaps by sex, revisited ----------------------------------------------

heat_male_only <- hagenheat2(t(male_dh), dist='binary', seriation_method='PCA_angle') +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  scale_colour_continuous() + 
  ggtitle('Males')

heat_female_only <- hagenheat2(t(female_dh), dist='binary', seriation_method='PCA_angle') +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  scale_colour_continuous() +
  ggtitle('Females')


# Sex-specific observations included --------------------------------------
# Next step: Males **only** (sex restriction) in the annotated column?

sex_spec_m <- df %>% 
  dplyr::select(textid, sex_restriction) %>% 
  right_join(data.frame(textid=row.names(male_dh), stringsAsFactors=FALSE), by='textid')

heat_male_restrict <- 
  hagenheat2(t(male_dh), dist='binary', seriation_method='PCA_angle', ann_col=sex_spec_m) +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  scale_colour_continuous() + 
  ggtitle('Males')

sex_spec_f <- df %>% 
  dplyr::select(textid, sex_restriction) %>% 
  right_join(data.frame(textid=row.names(female_dh), stringsAsFactors=FALSE), by='textid')
  
heat_female_restrict <- 
  hagenheat2(t(female_dh), dist='binary', seriation_method='PCA_angle', ann_col=sex_spec_f) +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  scale_colour_continuous() +
  ggtitle('Females')

## Complete heatmap with 12 sex-specific texts highlighted

df$sex_spec <- 0
df$sex_spec[df$male==1 & df$female==1 & df$sex_restriction==1] <- 1

sex_spec <- df %>% 
  dplyr::select(textid, sex_spec) %>% 
  right_join(data.frame(textid=row.names(dh), stringsAsFactors=FALSE), by='textid')

sex_specific_plot <- hagenheat2(t(dh), dist='binary', seriation_method='PCA_angle', ann_col=sex_spec) +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  scale_colour_continuous()
  
# Plots -------------------------------------------------------------------

male_lassoPlot
female_lassoPlot
heat_male_obs
heat_female_obs
Graph_evidence_females
Graph_evidence_males
heat_male_only
heat_female_only
heat_male_restrict
heat_female_restrict
sex_specific_plot

xtabs(~female+male+medicine, data=df)
xtabs(~female+medicine, data=df)


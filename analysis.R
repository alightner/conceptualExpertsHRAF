rm(list=ls())
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

# Descriptive stats -------------------------------------------------------

num_cultures <- length(table(df$culture_id))
txt_cases <- sum(df$case)
txt_models <- sum(df$model)

txt_casemodels <- sum(df$case+df$model>1)

num_txt_culture <- df %>% 
  group_by(culture_id) %>% 
  summarise(num_records_culture=length(textid))
num_txt_culture <- num_txt_culture$num_records_culture

max_txt_culture <- max(num_txt_culture)
min_txt_culture <- min(num_txt_culture)
median_txt_culture <- median(num_txt_culture)

min_year <- min(cite_data$year, na.rm=TRUE)
max_year <- max(cite_data$year, na.rm=TRUE)
median_year <- median(cite_data$year, na.rm=TRUE)
docs20cent <- round((sum(cite_data$year>1900, na.rm=TRUE)/length(cite_data$year[!is.na(cite_data$year)]))*100)

avg_texrec <- round(mean(texrec_length$numwords))
sd_texrec <- round(sd(texrec_length$numwords))
median_texrec <- median(texrec_length$numwords)
min_texrec <- min(texrec_length$numwords)
max_texrec <- max(texrec_length$numwords)

## Sex
male_perc <- round((sum(df$male)/nrow(df))*100)
female_perc <- round((sum(df$female)/nrow(df))*100)
male_female_perc <- round(sum(df$male+df$female>1)/nrow(df)*100)
unknown_sex_perc <- round(sum(df$male+df$female==0)/nrow(df)*100)
sex_restrict_perc <- round(sum(df$sex_restriction)/sum(df$male+df$female>0)*100)
sex_restrict_all <- round(mean(df$sex_restriction)*100)

male_only_perc <- round(mean(df$female==0 & df$male==1 & df$sex_restriction==1)*100)
female_only_perc <- round(mean(df$female==1 & df$male==0 & df$sex_restriction==1)*100)

male_prestige <- round(100*mean(df$prestige[df$male==1]))
female_prestige <- round(100*mean(df$prestige[df$female==1]))
male_teach <- round(100*mean(df$social_learning[df$male==1]))
female_teach <- round(100*mean(df$social_learning[df$female==1]))

## Age
child_adolescent <- round(sum(df$child_adolescent)/nrow(df)*100)
adult <- round(sum(df$adult)/nrow(df)*100)
older_elderly <- round(sum(df$older_elderly)/nrow(df)*100)
unknown_age_perc <- round(sum(df$child_adolescent+df$adult+df$older_elderly==0)/nrow(df)*100)

## Length of model variables in aggregate
len_modvars <- length(unique(c(hsm_vars, mpm_vars, anti_hsm, anti_mpm, ctm_vars, psm_vars, scm_vars, anti_scm)))

## When social learning is present, how often is it purchased? Learned by kin?
perc_learned_kin <- round(sum(df$social_learning==1 & df$learned_kin==1)/sum(df$social_learning==1)*100)
perc_learned_purchased <- round(sum(df$social_learning==1 & df$learned_purchased==1)/sum(df$social_learning==1)*100)

## binary matrix
bin_mat <- df[colnames(df) %in% c(hsm_vars, mpm_vars, anti_hsm, anti_mpm, ctm_vars, psm_vars, scm_vars, anti_scm)]

sl_perc <- round(mean(df$social_learning)*100)
learned_kin_perc <- round(mean(df$learned_kin[df$social_learning==1])*100)
learned_purchased_perc <- round(mean(df$learned_purchased[df$social_learning==1])*100)

collab_perc <- round(mean(df$experts_collaborate)*100)
compete_perc <- round(mean(df$experts_compete)*100)

perc_religious <- round(mean(df$religious_leader)*100)


# Domains from model scores -----------------------------------------------

source('glmer_domains.R')

# Evidence by domain ------------------------------------------------------

source('evidenceByDomain.R')

# Plotting the evidence by text and culture -------------------------------

# uncomment and run to re-create df_evidence.csv (can take about 10-20 minutes or so)
# source('compile-text-evidence.R')
df_evidence <- read_csv('data/df_evidence.csv')

source('model_scoring.R')   # computes model scores
df_evidence <- rbind(df_evidence, modScored)

# Cleans up for facets
df_evidence$mod_id <- str_replace_all(df_evidence$mod_id, pattern=' ', replacement='\n')
df_evidence$mod_id <- factor(df_evidence$mod_id)

# Re-ordering models (facets in plot) by model score
df_evidence$vars <- 
  factor(df_evidence$vars,
         levels=c(
           'Model score',
           unique(
             subset(
               df_evidence, vars!='Model score'
             )$vars[order(subset(
               df_evidence, vars!='Model score'
             )$estimate, decreasing=TRUE)], order=TRUE
           )
         )       
  )
df_evidence$vars <- factor(df_evidence$vars, levels=rev(levels(df_evidence$vars)))
df_evidence$mod_id <- 
  factor(df_evidence$mod_id,
       levels=df_evidence$mod_id[
         df_evidence$vars=='Model score'
         ][
           order(df_evidence$estimate[df_evidence$vars=='Model score'])
           ]  
)
df_evidence$mod_id <- factor(df_evidence$mod_id, levels=rev(levels(df_evidence$mod_id)))

# This is figure 3
Model_full_plot <- 
  ggplot(df_evidence, aes(x=estimate, y=vars, 
                          colour=type, 
                          alpha=factor(unique_vars))) +
  geom_point() +
  geom_errorbarh(aes(xmin=lowerCI, xmax=upperCI), height=0, lwd=1) +   #, alpha=0.4
  theme_bw(base_size=10) +
  facet_grid(mod_id~., space='free', scales='free') +
    scale_colour_manual(values=c(viridis::magma(11)[8], 
                                 '#008080',
                                 viridis::magma(11)[4])) +
  theme(strip.text.y=element_text(angle=0, hjust=0)) +
  scale_alpha_discrete(range=c(0.25,1), guide=FALSE) +
  scale_shape_manual(values=c(1,16), guide=FALSE) +
  labs(x='', y='', colour='') +
  scale_x_continuous(labels = function(x) paste0(x, "%"))

# Model scoring for main text ---------------------------------------------

ccm_est <- round(modScored$estimate[modScored$mod_id=='Collaborative cognition model'],1)
pkm_est <- round(modScored$estimate[modScored$mod_id=='Proprietary knowledge model'],1)
ctm_est <- round(modScored$estimate[modScored$mod_id=='Cultural transmission model'],1)
hsm_est <- round(modScored$estimate[modScored$mod_id=='Honest signaling model'],1)
mpm_est <- round(modScored$estimate[modScored$mod_id=='Mate provisioning model'],1)

anticcm_est <- round(modScored$estimate[modScored$mod_id=='Anti-collaborative cognition model'],1)
antihsm_est <- round(modScored$estimate[modScored$mod_id=='Anti-honest signaling model'],1)
antimpm_est <- round(modScored$estimate[modScored$mod_id=='Anti-mate provisioning model'],1)

# Fixed effects plot ------------------------------------------------------

# uncomment and run to re-create fixedEffects-domain.csv (takes some time)
# source('fixed-effects-domain.R')
fe_df <- read_csv('data/fixedEffects-domain.csv')

fe_df$model_specific <- ifelse(fe_df$unique_vars, 'specific', 'generic')
fe_df$mod_id <- str_replace_all(fe_df$mod_id, pattern=' ', replacement='\n')

# This is figure 4
fixed_effects_domain_plot <- 
  ggplot(unique(fe_df), aes(x=effect, y=reorder(vars, effect), colour=model_specific)) +
  facet_grid(mod_id~task, scales='free_y', space='free') +
  geom_point() +
  geom_errorbarh(aes(xmin=lowerCI, xmax=upperCI), height=0, lwd=1.75, alpha=0.4) +
  theme_bw(base_size=10) +
  scale_colour_manual(values=viridis::magma(11)[c(8,4)]) +
  geom_vline(xintercept=0, alpha=0.25) +
  scale_shape_manual(values=c(1,16), guide=FALSE) +
  labs(x='\nLog odds', y='', colour='Variable type') +
  theme(strip.text.y=element_text(angle=0, hjust=0))

# Clustering analysis -----------------------------------------------------

df <- cbind(
  data.frame(textid=df$textid, culture_id=df$culture_id, author_id=df$author_id,
             stringsAsFactors=FALSE),
  df[colnames(df) %in% c(hsm_vars, mpm_vars, anti_hsm, anti_mpm, ctm_vars, psm_vars, scm_vars, anti_scm)]
)
dh <- as.data.frame(df[sapply(df, is.numeric)])
row.names(dh) <- df$textid
dh <- dh[rowSums(dh)>0,]
colnames(dh) <- var_dict2[colnames(dh)]

# This is figure 5
heat_PCA_angle <- 
  hagenheat2(t(dh), dist='binary', seriation_method='PCA_angle') +
  theme(legend.position = "none", axis.text.x = element_blank()) +
    scale_colour_continuous()

# IRR ---------------------------------------------------------------------
source('IRR-check.R')   # Interrater reliability

# Minimum spanning tree vars ----------------------------------------------
source('spanning-tree.R')   # This generates figure 6


# Descriptive results -----------------------------------------------------

# How many vars per text-record?
avg_vpt <- mean(rowSums(dh))
median_vpt <- median(rowSums(dh))
sd_vpt <- sd(rowSums(dh))
max_vpt <- max(rowSums(dh))
min_vpt <- min(rowSums(dh))
lt10_vpt <- round(100*mean(rowSums(dh) <=10))
# hist(rowSums(dh))


# Variable table ----------------------------------------------------------

source('model_vars_table.R')



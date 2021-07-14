## **NOTE**: Add a discussion of this justification to the SI, with the dotchart and threshold = 26
## for the lasso regression when selecting predictors

# tmp <- d0[sapply(d0, is.numeric)]
# tmp <- tmp[colnames(tmp) %in% modlist2$vars]
# hist(colSums(tmp))
# dotchart(sort(colSums(tmp)))  >=26

# Tabular data by domain --------------------------------------------------

sdf <- read_csv('data/IRR-final-dataset.csv')
modlist2 <- data.table::rbindlist(
  list(data.frame(vars=hsm_vars, mod_id='hsm'),
       data.frame(vars=psm_vars, mod_id='psm'),
       data.frame(vars=ctm_vars, mod_id='ctm'),
       data.frame(vars=mpm_vars, mod_id='mpm'),
       data.frame(vars=scm_vars, mod_id='scm'))
)
# c = conceptual, t = motor, d = medicine
sdf <- sdf[sapply(sdf, is.numeric) & (colnames(sdf) %in% colnames(df))]
tab_data <- data.frame()
for(i in colnames(sdf)[colnames(sdf) %in% modlist2$vars]) {
  c0 <- sum(as.numeric(unlist(sdf[i]))[sdf$conceptual==0])
  c1 <- sum(as.numeric(unlist(sdf[i]))[sdf$conceptual==1])
  md0 <- sum(as.numeric(unlist(sdf[i]))[sdf$medicine==0])
  md1 <- sum(as.numeric(unlist(sdf[i]))[sdf$medicine==1])
  mt0 <- sum(as.numeric(unlist(sdf[i]))[sdf$motor==0])
  mt1 <- sum(as.numeric(unlist(sdf[i]))[sdf$motor==1])
  tab_data <- rbind(tab_data,
                    data.frame(variable=i, `domain present`=c1, `domain absent`=c0, domain='conceptual'),
                    data.frame(variable=i, `domain present`=md1, `domain absent`=md0, domain='medicine'),
                    data.frame(variable=i, `domain present`=mt1, `domain absent`=mt0, domain='motor skills')
  )
}

tab_data$variable <- var_dict2[tab_data$variable]
# tab_data <- tab_data[tab_data$variable %in% modlist2$vars,]
tab_data$total <- tab_data$domain.present+tab_data$domain.absent
tab_data <- tab_data %>% arrange(desc(total))

tab_data <- tab_data %>% pivot_longer(cols=starts_with('domain.'),
                                      names_to='pres_abs',
                                      names_prefix='domain.',
                                      values_to='support')

tab_data$text_color <- 0
tab_data$text_color[tab_data$support>90] <- 1

tab_data$pres_abs[tab_data$pres_abs=='present'] <- 'Domain\npresent'
tab_data$pres_abs[tab_data$pres_abs=='absent'] <- 'Domain\nabsent'

tab_data$domain[tab_data$domain=='conceptual'] <- paste0('conceptual (N=', sum(df$conceptual), ')')
tab_data$domain[tab_data$domain=='medicine'] <- paste0('medicine (N=', sum(df$medicine), ')')
tab_data$domain[tab_data$domain=='motor skills'] <- paste0('motor skills (N=', sum(df$motor), ')')

tabular_plot <- ggplot(subset(tab_data, total>=10), 
                       aes(x=pres_abs, y=reorder(variable, total), fill=support, label=support)) +
  geom_tile(alpha=0.8, colour='white') +
  geom_text(colour='white') +
  facet_wrap(~domain) +
  theme_bw() +
  scale_fill_gradient2(
    low='#110637',
    mid='#107449',
    high='#1add8a',
    midpoint=90
  ) +
  scale_colour_manual(values=c('white', 'black'), guide='none') +
  labs(x='', y='', fill='Evidence\n(variable)')


# Lasso regression --------------------------------------------------------
# 
# set.seed(2021)
# col_cutoff <- 26
# 
# d0 <- df[sapply(df, is.numeric)]
# d0$culture <- df$culture_id
# d0$author <- df$author_id
# d0$medicine <- NULL
# d0$conceptual <- NULL
# d0$motor <- NULL
# d0$medicine <- df$medicine
# d0$conceptual <- df$conceptual
# d0$motor <- df$motor
# 
# # lasso_mod <- function() {
# #
# # }
# # save(ldf, file='data/lasso_DF')
# ## Conceptual domains
# y <- d0$conceptual
# x <- d0[sapply(d0, is.numeric)] %>%
#   dplyr::select(-c(medicine, conceptual, motor)) %>%
#   dplyr::select(any_of(unique(modlist2$vars))) %>%
#   as.matrix()
# x <- x[,colSums(x)>=col_cutoff]
# conceptual_m <- cv.glmnet(x=x, y=y, family='binomial', alpha=1, relax=TRUE, trace=TRUE, standardize=FALSE)
# coefs2 <- coef(conceptual_m, s=conceptual_m$lambda.min)
# coefs2 <- coefs2[-which(rownames(coefs2)=='(Intercept)'),]
# names(coefs2) <- var_dict2[names(coefs2)]
# coefs_conceptual <- coefs2
# 
# beta_hat <- coef(conceptual_m, x=x, y=y, s=conceptual_m$lambda.min/nrow(x), exact=TRUE)
# out <- fixedLassoInf(x, y, beta_hat, conceptual_m$lambda.min, family='binomial')
# # out$vars[out$pv<=0.05]
# se_conceptual <- data.frame(vars=names(out$vars), out$ci)
# colnames(se_conceptual) <- c('name', 'lowerCI', 'upperCI')
# se_conceptual$name <- var_dict2[se_conceptual$name]
# 
# ## Medicinal domains
# y <- d0$medicine
# x <- d0[sapply(d0, is.numeric)] %>%
#   dplyr::select(-c(medicine, conceptual, motor)) %>%
#   dplyr::select(any_of(unique(modlist2$vars))) %>%
#   as.matrix()
# x <- x[,colSums(x)>=col_cutoff]
# medicine_m <- cv.glmnet(x=x, y=y, family='binomial', alpha=1, relax=TRUE, trace=TRUE, standardize=FALSE)
# coefs2 <- coef(medicine_m, s=medicine_m$lambda.min)
# coefs2 <- coefs2[-which(rownames(coefs2)=='(Intercept)'),]
# names(coefs2) <- var_dict2[names(coefs2)]
# coefs_medicine <- coefs2
# 
# beta_hat <- coef(medicine_m, x=x, y=y, s=medicine_m$lambda.min/nrow(x), exact=TRUE)
# out <- fixedLassoInf(x, y, beta_hat, medicine_m$lambda.min, family='binomial')
# # out$vars[out$pv<=0.05]
# se_medicine <- data.frame(vars=names(out$vars), out$ci)
# colnames(se_medicine) <- c('name', 'lowerCI', 'upperCI')
# se_medicine$name <- var_dict2[se_medicine$name]
# 
# ## Motor domains
# y <- d0$motor
# x <- d0[sapply(d0, is.numeric)] %>%
#   dplyr::select(-c(medicine, conceptual, motor)) %>%
#   dplyr::select(any_of(unique(modlist2$vars))) %>%
#   as.matrix()
# x <- x[,colSums(x)>=col_cutoff]
# motor_m <- cv.glmnet(x=x, y=y, family='binomial', alpha=1, relax=TRUE, trace=TRUE, standardize=FALSE)
# coefs2 <- coef(motor_m, s=motor_m$lambda.min)
# coefs2 <- coefs2[-which(rownames(coefs2)=='(Intercept)'),]
# names(coefs2) <- var_dict2[names(coefs2)]
# coefs_motor <- coefs2
# 
# beta_hat <- coef(motor_m, x=x, y=y, s=motor_m$lambda.min/nrow(x), exact=TRUE)
# out <- fixedLassoInf(x, y, beta_hat, motor_m$lambda.min, family='binomial')
# # out$vars[out$pv<=0.05]
# se_motor <- data.frame(vars=names(out$vars), out$ci)
# colnames(se_motor) <- c('name', 'lowerCI', 'upperCI')
# se_motor$name <- var_dict2[se_motor$name]
# 
# se_df <- bind_rows(list(conceptual=se_conceptual, medicine=se_medicine, motor=se_motor), .id='domain')
# 
# ldf <-
#   bind_rows(list(conceptual=coefs_conceptual, medicine=coefs_medicine, motor=coefs_motor), .id='domain') %>%
#   pivot_longer(-domain) %>%
#   left_join(se_df, by=c('domain', 'name')) %>%
#   dplyr::filter(value!=0)
# 
# ldf$name[ldf$name=="Knowledge distributed/multiple experts" ] <- "Knowledge distributed/\nmultiple experts"
# ldf$name[ldf$name=="Cares about reputation"] <- "Cares about\nreputation"
# ldf$name[ldf$name=="Patronage based on efficacy"] <- "Patronage based\non efficacy"
# ldf$name[ldf$name=="Knowledge domain is widespread"] <- "Knowledge domain\nis widespread"
# ldf$name[ldf$name=="Assists with a common problem"] <- "Assists with a\ncommon problem"
# ldf$name[ldf$name=="Narrow specialization"] <- "Narrow\nspecialisation"
# ldf$name[ldf$name=="Hierarchy w/in domain of expertise"] <- "Hierarchy w/in\ndomain of expertise"
# ldf$name[ldf$name=="Assists with uncommon/serious problem" ] <- "Assists with uncommon/\nserious problem"
# ldf$name[ldf$name=="Reputation for efficacy"] <- "Reputation for\nefficacy"
# ldf$domain[ldf$domain=='motor'] <- 'motor skills'

load('data/lasso_DF2')
lassoDomain_plot <- 
  ggplot(ldf, aes(x=exp(value), y=reorder_within(name, value, domain))) +
  geom_point(size=2.25) +
    geom_errorbarh(aes(xmin=exp(lowerCI), xmax=exp(upperCI)), height=0.1, lwd=0.75, alpha=1) +
  geom_vline(xintercept=1, alpha=0.45) +
  facet_wrap(~domain, scales='free') +
  scale_y_reordered() +
  scale_x_log10() +
  theme_bw() +
  labs(x='\nOdds ratio', y='')



# Fixed effect: male --------------------------------------------------

formula_string = "{outcome} ~ male + (1|culture/author)"

d0 <- df[sapply(df, is.numeric)]
d0$culture <- df$culture_id
d0$author <- df$author_id
d0$male <- NULL
d0$male <- df$male
mod_record_male <- textrecord_support(d0, formula_string, nonNum=3)

fe_male <- data.frame(
  vars=mod_record_male$vars,
  effect=NA,
  lowerCI=NA,
  upperCI=NA,
  stringsAsFactors = FALSE
)

fe_male$effect <- unlist(lapply(mod_record_male$Tidy, function(x) x$estimate[2]))
fe_male$lowerCI <- unlist(lapply(mod_record_male$Tidy, function(x) x$conf.low[2]))
fe_male$upperCI <- unlist(lapply(mod_record_male$Tidy, function(x) x$conf.high[2]))

modlist2 <- data.table::rbindlist(
  list(data.frame(vars=hsm_vars, mod_id='hsm'),
       data.frame(vars=psm_vars, mod_id='psm'),
       data.frame(vars=ctm_vars, mod_id='ctm'),
       data.frame(vars=mpm_vars, mod_id='mpm'),
       data.frame(vars=scm_vars, mod_id='scm'))
)

fe_male2 <- fe_male %>% left_join(modlist2, by='vars')
sum_male_support <- colSums(df[fe_male2$vars])/nrow(df)

fe_male2 <- fe_male2 %>% left_join(data.frame(vars=names(sum_male_support),
                                            omit=sum_male_support<0.1), by='vars')

fe_male2$vars <- var_dict2[fe_male2$vars]
fe_male2$mod_id <- mod_dict[fe_male2$mod_id]

var_tmp <- data.frame(table(fe_male2$vars), stringsAsFactors=FALSE)
unique_vars <- as.character(var_tmp$Var1[var_tmp$Freq==1])
fe_male2$unique_vars <- fe_male2$vars %in% unique_vars 

fe_male2 <- fe_male2[fe_male2$omit==FALSE,]

# Fixed effect: female --------------------------------------------------

formula_string = "{outcome} ~ female + (1|culture/author)"

d0 <- df[sapply(df, is.numeric)]
d0$culture <- df$culture_id
d0$author <- df$author_id
d0$female <- NULL
d0$female <- df$female
mod_record_female <- textrecord_support(d0, formula_string, nonNum=3)

fe_female <- data.frame(
  vars=mod_record_female$vars,
  effect=NA,
  lowerCI=NA,
  upperCI=NA,
  stringsAsFactors = FALSE
)

fe_female$effect <- unlist(lapply(mod_record_female$Tidy, function(x) x$estimate[2]))
fe_female$lowerCI <- unlist(lapply(mod_record_female$Tidy, function(x) x$conf.low[2]))
fe_female$upperCI <- unlist(lapply(mod_record_female$Tidy, function(x) x$conf.high[2]))

modlist2 <- data.table::rbindlist(
  list(data.frame(vars=hsm_vars, mod_id='hsm'),
       data.frame(vars=psm_vars, mod_id='psm'),
       data.frame(vars=ctm_vars, mod_id='ctm'),
       data.frame(vars=mpm_vars, mod_id='mpm'),
       data.frame(vars=scm_vars, mod_id='scm'))
)

fe_female2 <- fe_female %>% left_join(modlist2, by='vars')
sum_female_support <- colSums(df[fe_female2$vars])/nrow(df)

fe_female2 <- fe_female2 %>% left_join(data.frame(vars=names(sum_female_support),
                                              omit=sum_female_support<0.1), by='vars')

fe_female2$vars <- var_dict2[fe_female2$vars]
fe_female2$mod_id <- mod_dict[fe_female2$mod_id]

var_tmp <- data.frame(table(fe_female2$vars), stringsAsFactors=FALSE)
unique_vars <- as.character(var_tmp$Var1[var_tmp$Freq==1])
fe_female2$unique_vars <- fe_female2$vars %in% unique_vars 

fe_female2 <- fe_female2[fe_female2$omit==FALSE,]

# Fixed effects plot (by domain category) --------------------------------------

fe_male2$sex <- 'male'
fe_female2$sex <- 'female'

fe_sexdf <- rbindlist(list(
  fe_male2,
  fe_female2
))
fe_sexdf$omit <- NULL

fe_sexdf <- fe_sexdf %>% 
  dplyr::filter(
    !is.na(mod_id)
    #unique_vars==TRUE
  )

# fe_df$task <- factor(fe_df$task, levels=c('medicine', 'conceptual', 'motor skills'))
# write.table(fe_df, file='data/fixedEffects-domain.csv', sep=',', row.names=FALSE)


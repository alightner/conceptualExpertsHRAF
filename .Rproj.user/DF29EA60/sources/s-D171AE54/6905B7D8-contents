# Fixed effect: medicine --------------------------------------------------

formula_string = "{outcome} ~ medicine + (1|culture/author)"

d0 <- df[sapply(df, is.numeric)]
d0$culture <- df$culture_id
d0$author <- df$author_id
d0$medicine <- NULL
d0$medicine <- df$medicine
mod_record_medicine <- textrecord_support(d0, formula_string, nonNum=3)

fe_med <- data.frame(
  vars=mod_record_medicine$vars,
  effect=NA,
  lowerCI=NA,
  upperCI=NA,
  stringsAsFactors = FALSE
)

fe_med$effect <- unlist(lapply(mod_record_medicine$Tidy, function(x) x$estimate[2]))
fe_med$lowerCI <- unlist(lapply(mod_record_medicine$Tidy, function(x) x$conf.low[2]))
fe_med$upperCI <- unlist(lapply(mod_record_medicine$Tidy, function(x) x$conf.high[2]))

modlist2 <- data.table::rbindlist(
  list(data.frame(vars=hsm_vars, mod_id='hsm'),
       data.frame(vars=psm_vars, mod_id='psm'),
       data.frame(vars=ctm_vars, mod_id='ctm'),
       data.frame(vars=mpm_vars, mod_id='mpm'),
       data.frame(vars=scm_vars, mod_id='scm'))
)

fe_med2 <- fe_med %>% left_join(modlist2, by='vars')
sum_med_support <- colSums(df[fe_med2$vars])/nrow(df)

fe_med2 <- fe_med2 %>% left_join(data.frame(vars=names(sum_med_support),
                                            omit=sum_med_support<0.1), by='vars')

fe_med2$vars <- var_dict2[fe_med2$vars]
fe_med2$mod_id <- mod_dict[fe_med2$mod_id]

var_tmp <- data.frame(table(fe_med2$vars), stringsAsFactors=FALSE)
unique_vars <- as.character(var_tmp$Var1[var_tmp$Freq==1])
fe_med2$unique_vars <- fe_med2$vars %in% unique_vars 

fe_med2 <- fe_med2[fe_med2$omit==FALSE,]

# Fixed effect: ethnoscience --------------------------------------------------

formula_string = "{outcome} ~ conceptual + (1|culture/author)"

d0 <- df[sapply(df, is.numeric)]
d0$culture <- df$culture_id
d0$author <- df$author_id
d0$conceptual <- NULL
d0$conceptual <- df$conceptual
mod_record_ethno <- textrecord_support(d0, formula_string, nonNum=3)

fe_ethno <- data.frame(
  vars=mod_record_ethno$vars,
  effect=NA,
  lowerCI=NA,
  upperCI=NA,
  stringsAsFactors = FALSE
)

fe_ethno$effect <- unlist(lapply(mod_record_ethno$Tidy, function(x) x$estimate[2]))
fe_ethno$lowerCI <- unlist(lapply(mod_record_ethno$Tidy, function(x) x$conf.low[2]))
fe_ethno$upperCI <- unlist(lapply(mod_record_ethno$Tidy, function(x) x$conf.high[2]))

fe_ethno2 <- fe_ethno %>% left_join(modlist2, by='vars')
sum_ethno_support <- colSums(df[fe_ethno2$vars])/nrow(df)

fe_ethno2 <- fe_ethno2 %>% left_join(data.frame(vars=names(sum_ethno_support),
                                                omit=sum_ethno_support<0.1), by='vars')

fe_ethno2$vars <- var_dict2[fe_ethno2$vars]
fe_ethno2$mod_id <- mod_dict[fe_ethno2$mod_id]

var_tmp <- data.frame(table(fe_ethno2$vars), stringsAsFactors=FALSE)
unique_vars <- as.character(var_tmp$Var1[var_tmp$Freq==1])
fe_ethno2$unique_vars <- fe_ethno2$vars %in% unique_vars 

fe_ethno2 <- fe_ethno2[fe_ethno2$omit==FALSE,]

# Fixed effect: motor skills --------------------------------------------------

formula_string = "{outcome} ~ motor + (1|culture/author)"

d0 <- df[sapply(df, is.numeric)]
d0$culture <- df$culture_id
d0$author <- df$author_id
d0$motor <- NULL
d0$motor <- df$motor
mod_record_motor <- textrecord_support(d0, formula_string, nonNum=3)

fe_motor <- data.frame(
  vars=mod_record_motor$vars,
  effect=NA,
  lowerCI=NA,
  upperCI=NA,
  stringsAsFactors = FALSE
)

fe_motor$effect <- unlist(lapply(mod_record_motor$Tidy, function(x) x$estimate[2]))
fe_motor$lowerCI <- unlist(lapply(mod_record_motor$Tidy, function(x) x$conf.low[2]))
fe_motor$upperCI <- unlist(lapply(mod_record_motor$Tidy, function(x) x$conf.high[2]))

fe_motor2 <- fe_motor %>% left_join(modlist2, by='vars')
sum_motor_support <- colSums(df[fe_motor2$vars])/nrow(df)

fe_motor2 <- fe_motor2 %>% left_join(data.frame(vars=names(sum_motor_support),
                                                omit=sum_motor_support<0.1), by='vars')

fe_motor2$vars <- var_dict2[fe_motor2$vars]
fe_motor2$mod_id <- mod_dict[fe_motor2$mod_id]

var_tmp <- data.frame(table(fe_motor2$vars), stringsAsFactors=FALSE)
unique_vars <- as.character(var_tmp$Var1[var_tmp$Freq==1])
fe_motor2$unique_vars <- fe_motor2$vars %in% unique_vars 

fe_motor2 <- fe_motor2[fe_motor2$omit==FALSE,]

# Fixed effects plot (by domain category) --------------------------------------

fe_med2$task <- 'medicine'
fe_ethno2$task <- 'conceptual'
fe_motor2$task <- 'motor skills'

fe_df <- rbindlist(list(
  fe_med2,
  fe_ethno2,
  fe_motor2
))
fe_df$omit <- NULL

fe_df <- fe_df %>% 
  dplyr::filter(
    !is.na(mod_id)
    #unique_vars==TRUE
  )

fe_df$task <- factor(fe_df$task, levels=c('medicine', 'conceptual', 'motor skills'))

write.table(fe_df, file='data/fixedEffects-domain.csv', sep=',', row.names=FALSE)


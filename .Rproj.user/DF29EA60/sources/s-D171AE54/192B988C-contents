# Variable SUPPORT by TEXT RECORD --------------------------------------------

formula_string = "{outcome} ~ 1 + (1|culture/author)"

d0 <- df[sapply(df, is.numeric)]
d0$culture <- df$culture_id
d0$author <- df$author_id
mod_record <- textrecord_support(d0, formula_string)

tm_support <- data.frame(
  vars=mod_record$vars,
  estimate=mod_record$Estimate,
  lowerCI=mod_record$lowerCI,
  upperCI=mod_record$upperCI,
  stringsAsFactors = FALSE
)

modlist2 <- data.table::rbindlist(
  list(data.frame(vars=hsm_vars, mod_id='hsm'),
       data.frame(vars=psm_vars, mod_id='psm'),
       data.frame(vars=ctm_vars, mod_id='ctm'),
       data.frame(vars=mpm_vars, mod_id='mpm'),
       data.frame(vars=scm_vars, mod_id='scm'))
)

tm_support2 <- tm_support %>% left_join(modlist2, by='vars')
var_tmp <- data.frame(table(tm_support2$vars), stringsAsFactors=FALSE)
unique_vars <- as.character(var_tmp$Var1[var_tmp$Freq==1])
tm_support2$unique_vars <- tm_support2$vars %in% unique_vars 

# Variable SUPPORT by CULTURE ------------------------------------------------

d0 <- cbind(
  data.frame(
    culture_id=df$culture_id,
    author_id=df$author_id,
    stringsAsFactors=FALSE
  ),
  df[sapply(df, is.numeric)]
)

c_trials <- 1e3
c_mat <- matrix(NA, nrow=c_trials, ncol=ncol(d0[sapply(d0, is.numeric)]))

for(i in 1:c_trials){
  c_mat[i,] <- unlist(sapply(
    # resample(d0, 'culture_id', replace=TRUE),    # use this to speed up resample
    resample(d0, c('culture_id', 'author_id'), replace=c(TRUE,TRUE)),
    function(x) if(is.numeric(x)) mean(x)
  ))
}

c_mat2 <- as.data.frame(c_mat)
colnames(c_mat2) <- colnames(d0[sapply(d0, is.numeric)])
sdlist <- sapply(c_mat2, sd)

d0 <- df %>% 
  group_by(culture_id) %>% 
  summarise(across(where(is.numeric), sum)) 
d01 <- d0[sapply(d0, is.numeric)]
d01[d01>0] <- 1
d0[sapply(d0, is.numeric)] <- d01
tmp_rec <- tibble(culture_id=culture_df$culture_id, records=culture_df$records)
d0 <- d0 %>% left_join(tmp_rec, by='culture_id')

c_ts <- tibble(
  vars=colnames(d0[-c(1,length(d0))]),
  c_estimate=colMeans(d0[-c(1,length(d0))]),
  c_sd=sdlist
)
c_ts$c_upperCI <- c_ts$c_estimate + 2*c_ts$c_sd
c_ts$c_lowerCI <- c_ts$c_estimate - 2*c_ts$c_sd
c_ts$c_upperCI[c_ts$c_upperCI>1]<-1
c_ts$c_lowerCI[c_ts$c_lowerCI<0]<-0

tm_support2 <- tm_support2 %>% left_join(c_ts, by='vars')
tm_support2$vars <- var_dict2[tm_support2$vars]
tm_support2$mod_id <- mod_dict[tm_support2$mod_id]

## reshaping for easier plotting
tm1 <- tm_support2 %>% 
  tibble() %>% 
  dplyr::filter(!is.na(mod_id)) %>% 
  dplyr::select(
    vars:unique_vars
  )

tm2 <- tm_support2 %>% 
  tibble() %>% 
  dplyr::filter(!is.na(mod_id)) %>% 
  dplyr::select(
    vars,
    estimate=c_estimate,
    lowerCI=c_lowerCI,
    upperCI=c_upperCI,
    mod_id,
    unique_vars
  )
tm1$type <- 'text'
tm2$type <- 'culture'
tmdf <- data.table::rbindlist(list(tm1,tm2))
df_support <- tmdf

# Variable AGAINST by TEXT RECORD --------------------------------------------

formula_string = "{outcome} ~ 1 + (1|culture/author)"

tm_against <- data.frame(
  vars=mod_record$vars,
  estimate=mod_record$Estimate,
  lowerCI=mod_record$lowerCI,
  upperCI=mod_record$upperCI,
  stringsAsFactors = FALSE
)

modlist2 <- data.table::rbindlist(
  list(data.frame(vars=anti_hsm, mod_id='antihsm'),
       data.frame(vars=anti_scm, mod_id='antiscm'),
       data.frame(vars=anti_mpm, mod_id='antimpm'))
)

tm_against2 <- tm_against %>% left_join(modlist2, by='vars')
var_tmp <- data.frame(table(tm_against2$vars), stringsAsFactors=FALSE)
unique_vars <- as.character(var_tmp$Var1[var_tmp$Freq==1])
tm_against2$unique_vars <- tm_against2$vars %in% unique_vars 

# Variable AGAINST by CULTURE ------------------------------------------------

tm_against2 <- tm_against2 %>% left_join(c_ts, by='vars')

tm_against2$vars <- var_dict2[tm_against2$vars]
tm_against2$mod_id <- mod_dict[tm_against2$mod_id]

## reshaping for easier plotting
tm1 <- tm_against2 %>% 
  tibble() %>% 
  dplyr::filter(!is.na(mod_id)) %>% 
  dplyr::select(
    vars:unique_vars
  )

tm2 <- tm_against2 %>% 
  tibble() %>% 
  dplyr::filter(!is.na(mod_id)) %>% 
  dplyr::select(
    vars,
    estimate=c_estimate,
    lowerCI=c_lowerCI,
    upperCI=c_upperCI,
    mod_id,
    unique_vars
  )
tm1$type <- 'text'
tm2$type <- 'culture'
tmdf <- data.table::rbindlist(list(tm1,tm2))

df_against <- tmdf

# Model COMBINED by TEXT and CULTURE --------------------------------------

df_evidence <- rbind(df_support, df_against)

df_evidence$mod_id <- factor(df_evidence$mod_id, levels=c(
  "Honest signaling model",
  "Anti-honest signaling model",
  "Mate provisioning model",
  "Anti-mate provisioning model",
  "Cultural transmission model",
  "Proprietary knowledge model",
  "Collaborative cognition model",
  "Anti-collaborative cognition model"
))

df_evidence$estimate <- df_evidence$estimate*100
df_evidence$lowerCI <- df_evidence$lowerCI*100
df_evidence$upperCI <- df_evidence$upperCI*100

write.table(df_evidence, file='data/df_evidence.csv', sep=',', row.names=FALSE)

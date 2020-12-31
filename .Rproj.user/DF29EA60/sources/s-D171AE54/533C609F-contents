models <- list(
  list('Honest signaling model', hsm_vars),
  list('Proprietary knowledge model', psm_vars),
  list('Cultural transmission model', ctm_vars),
  list('Mate provisioning model', mpm_vars),
  list('Collaborative cognition model', scm_vars),
  list('Anti-honest signaling model', anti_hsm),
  list('Anti-mate provisioning model', anti_mpm),
  list('Anti-collaborative cognition model', anti_scm)
)

modScoreList <- lapply(models, 
       function(m)
       {
         model <- m[[1]]
         model_vars <- m[[2]]
         w <- rep(length(model_vars), nrow(df))
         
         if(sum(as.matrix(df[,model_vars])) !=0){
           m2 <- glmer(rowSums(df[,model_vars])/w ~ 1+(1|culture_id/author_id), family=binomial, weights=w, data=df, nAGQ=0)
           # model
           estimate <- logit.inv(fixef(m2)[['(Intercept)']])*100
           ci <- logit.inv(confint(m2, method='Wald')['(Intercept)',])
           lowerCI <- ci[[1]]*100
           upperCI <- ci[[2]]*100
           
         } else {
           estimate <- 0
           lowerCI <- 0
           upperCI <- 0
         }
         model_d <- data.frame(estimate, lowerCI, upperCI, mod_id=model)
         return(model_d)
       }
)
    
modScored <- data.table::rbindlist(modScoreList)  
modScored$vars <- 'Model score'
modScored$unique_vars <- TRUE
modScored$type <- 'model score'
modScored <- modScored %>% dplyr::select(
  vars, estimate, lowerCI, upperCI, mod_id, unique_vars, type
)



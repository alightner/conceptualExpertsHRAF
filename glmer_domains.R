
df$ccm_score <- rowSums(df[scm_vars])/length(scm_vars)
df$hsm_score <- rowSums(df[hsm_vars])/length(hsm_vars)
df$mpm_score <- rowSums(df[mpm_vars])/length(mpm_vars)
df$pkm_score <- rowSums(df[psm_vars])/length(psm_vars)
df$ctm_score <- rowSums(df[ctm_vars])/length(ctm_vars)

# summary(glm(medicine ~ pkm_score+ctm_score+ccm_score+hsm_score+mpm_score,
#             data=df, family='binomial'))

glmer_m1 <- glmer(medicine ~ pkm_score+ctm_score+ccm_score+hsm_score+mpm_score + (1|culture_id/author_id),
      data=df, family='binomial')

glmer_m2 <- glmer(conceptual ~ pkm_score+ctm_score+ccm_score+hsm_score+mpm_score + (1|culture_id/author_id),
           data=df, family='binomial')

glmer_m3 <- glmer(motor ~ pkm_score+ctm_score+ccm_score+hsm_score+mpm_score + (1|culture_id/author_id),
            data=df, family='binomial')

glmer_data <- bind_rows(list(medicine=broom.mixed::tidy(glmer_m1, conf.int=T),
               conceptual=broom.mixed::tidy(glmer_m2, conf.int=T),
               motor=broom.mixed::tidy(glmer_m3, conf.int=T)), .id='domain') %>% 
  dplyr::filter(effect=='fixed', term!='(Intercept)')

glmer_data$term[glmer_data$term=='pkm_score'] <- 'Propietary knowledge\nmodel score'
glmer_data$term[glmer_data$term=='ctm_score'] <- 'Cultural transmission\nmodel score'
glmer_data$term[glmer_data$term=='hsm_score'] <- 'Honest signalling\nmodel score'
glmer_data$term[glmer_data$term=='ccm_score'] <- 'Collaborative cognition\nmodel score'
glmer_data$term[glmer_data$term=='mpm_score'] <- 'Mate provisioning\nmodel score'

glmer_domain_plot <- 
  ggplot(glmer_data, aes(x=estimate, y=term, xmin=conf.low, xmax=conf.high)) +
    geom_point(size=2.25) +
    geom_errorbarh(height=0.1, lwd=0.75, alpha=1) +
  #geom_pointrange() +
  facet_grid(~domain) +
  theme_bw() +
  labs(x='\nLog odds', y='') +
  geom_vline(xintercept = 0, alpha=0.45)

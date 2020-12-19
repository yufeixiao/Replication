#code adapted from Mohamed, Sarah; Daponte-Smith, Noah; Rasband, Reed, 2019, "Replication Data for: Improving Treatment Comparability in an Instrumental Variables Design: Replication and Extension of Hangartner et. al 2018", https://doi.org/10.7910/DVN/MGUYVO, Harvard Dataverse, V1, UNF:6:8aJ3EtVnIUgOyFiOaQVMxA== [fileUNF]

library(tidyverse)
library(haven)
library(glue)
library(MatchIt)
library(Matching)
library(MatchingFrontier)
library(AER)
library(Amelia)
library(lattice)
library(clusterSEs)


setwd("YOUR PATH HERE")
survey_data <- read_dta("survey_data.dta")
survey_data$mi_m <- survey_data$`_mi_m`


##########################
#FIGURE 4 IN ORIGINAL
##########################

#data

holder_template <- data.frame(score_asylum = rep(NA, 4), # fewer asylum seekers
                              asylumspec_kids = rep(NA,4), # ban from schools 
                              asylumgen = rep(NA,4),  # are a burden
                              asylumspec_terrorism = rep(NA,4), # more crimes
                              asylumspec_criminality  = rep(NA,4),  # more terror attacs
                              asylumspec_burden = rep(NA,4), # component 
                              score_immig = rep(NA, 4), # immigration component
                              incrdecr_economicimmigr = rep(NA, 4),  # fewer economic migrants
                              borders_bordercontrol = rep(NA,4),  # increase border protection
                              diff_muslim = rep(NA,4),  # T(Muslim - Muslim immi)
                              diff_x = rep(NA,4), # T(Christ - Christ immi)
                              score_muslim = rep(NA, 4), # Muslim component
                              incrdecr_minority = rep(NA,4), # decrease representation
                              diff_immig = rep(NA, 4), # T (Christ immi - Muslim immi)
                              islam_acquainted = rep(NA, 4), # how many do not integrate
                              islam_extremism = rep(NA, 4), # how many support extremists
                              score_behavioral = rep(NA, 4), #Behavioral component
                              notifyMPs_rec = rep(NA, 4), # Notify MP? 
                              donation_bin = rep(NA, 4), # Donate?
                              donation_self = rep(NA, 4), # 100-donation 
                              petition_rec = rep(NA, 4), # Sign Petition?
                              score_all = rep(NA, 4)
)

holder_figure4 <- holder_template

varlist <- c(  "score_asylum",
               "asylumspec_kids" ,
               "asylumgen" ,
               "asylumspec_terrorism",
               "asylumspec_criminality" ,
               "asylumspec_burden" ,
               "score_immig", 
               "incrdecr_economicimmigr" ,
               "borders_bordercontrol" ,
               "diff_muslim", 
               "diff_x" ,
               "score_muslim" ,
               "incrdecr_minority" ,
               "diff_immig" ,
               "islam_acquainted", 
               "islam_extremism" ,
               "score_behavioral" ,
               "notifyMPs_rec",
               "donation_bin" ,
               "donation_self" ,
               "petition_rec",
               "score_all")

for ( j in varlist ) { 
  b.out<-NULL
  se.out<-NULL
  for(i in 1:5) {
    reg.data <- survey_data %>% filter(mi_m == i) 
    iv.out <- ivreg(data = reg.data, reg.data[[j]] ~ treatment + female + age + as.factor(edu)| 
                      logdist + female + age + as.factor(edu), weights = weight2_trim)
    cluster.se <- sqrt(diag(vcovCL(iv.out, cluster = ~munid, type = "HC0",
                                   target = NULL, inverse_var = FALSE)))
    b.out <- rbind(b.out, iv.out$coef)
    se.out <- rbind(se.out, cluster.se)
  }
  combined.results <- mi.meld(q = b.out, se = se.out)
  combined.results
  
  holder_figure4[1,j] <- combined.results$q.mi[2]
  holder_figure4[2,j] <- combined.results$se.mi[2]
  holder_figure4[3,j] <- combined.results$q.mi[1]
  holder_figure4[4,j] <- combined.results$se.mi[1]
}

fig.4.data <- as.data.frame(t(holder_figure4))
colnames(fig.4.data) <- c("beta_1", "se_1", "beta_0", "se_0")

fig.4.data$lo <- fig.4.data$beta_1 + fig.4.data$se_1 * -1.96 
fig.4.data$hi <- fig.4.data$beta_1 + fig.4.data$se_1 * 1.96 

fig.4.data$vars <- rev(c( "Asylum-seeker component (SD=1)",
                          "Ban from schools (1-5)" ,
                          "Fewer asylum-seekers (1-5)" ,
                          "More terror attacks (1-5)",
                          "More crimes (1-5)" ,
                          "Are a burden (1-5)" ,
                          "Immigrant component (SD=1)", 
                          "Fewer economic migrants (1-5)" ,
                          "Increase border protection (1-5)" ,
                          "T(Muslim-Muslim immi.) (1-5)", 
                          "T(Christ.-Christ. immi.) (1-5)" ,
                          "Muslim Component (SD=1)" ,
                          "Decrease representation (1-5)" ,
                          "T(Christ. immi.-Muslim immi.) (1-5)" ,
                          "How many do not integrate (1-5)",
                          "How many support extremists (1-5)" ,
                          "Behavioral component (SD=1)" ,
                          "Notify MP? (-2,2)",
                          "Donate? (0,1)" ,
                          "(100-donation)/100 (0-1)" ,
                          "Sign petition? (0,1)",
                          "All Scores"))
fig.4.data$color <- c(1,2,2,3,3,3,1,2,2,3,3,1,2,3,3,3,1,2,2,2,2,1) # gives the right colors

fig.4.data$vars <- as.character(fig.4.data$vars) #These lines help order the var list for ggplot
fig.4.data$vars <- rev(factor(fig.4.data$vars, levels=unique(fig.4.data$vars)))


fig.4.plot <- ggplot(data = fig.4.data, aes(x = vars, y = beta_1, color = as.factor(color))) + 
  geom_point() + 
  theme_minimal()+ 
  geom_linerange(aes(ymin = lo, ymax = hi)) + coord_flip() + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
  theme(legend.position="none")+
  geom_hline(yintercept = 0, linetype = "dotted")+
  scale_color_manual(breaks = c("1", "2", "3"), 
                     values = c("#000000", "#5599ff","#00bb00"))+
  ylab("")+
  xlab("")  

ggsave(plot=fig.4.plot, "figure4.pdf", width=(6.3228344444445), height=(6.3228344444445/4)*3, units="in")

fig.4.plot

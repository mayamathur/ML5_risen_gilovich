
# Calculate point estimates for the overarching analyses
main.dir = "~/Dropbox/Personal computer/Independent studies/Hedges response/Linked to OSF (Hedges)"
ml5.data.dir = paste( main.dir, "ML5 data", sep = "/")
code.dir = paste( main.dir, "Code", sep = "/")

# get code
setwd(code.dir)  
source("ml5_helper.R")  # for prepping ML5 data
source("helper.R")
source("bootfuns.R")



# subject-level dataset
setwd(ml5.data.dir)
library(readr)
b = read_csv("full_prepped_dataset.csv")

library(testthat)
expect_equal( nrow(b), 4441 ) 

############################# SITE-LEVEL ESTIMATES #############################

# summarize subject-level data by site
# slightly modified from Mathur et al. (2018)'s code
library(dplyr)
library(purrr)
library(metafor)

sites = b %>%
  select(-starts_with("site.")) %>%
  split(.$site) %>%
  
  # d here is the site dataset
  map_df(function (d) {
    
    # initialize site dataframe
    site_result <- data_frame(site = d$site[1], 
                              group = d$group[1], 
                              is.mturk = ifelse(group == "a.mturk", TRUE, FALSE))
    
    # do work on each site dataframe
    # same code as in data_prep function
    m = lm(lkl ~ tempt * load, data=d)
    CIs = confint(m)
    
    site_result$site.int.est = coef(m)["tempt:load"]
    site_result$site.int.pval = summary(m)$coefficients["tempt:load","Pr(>|t|)"]
    site_result$site.int.SE = sqrt( vcov(m)["tempt:load", "tempt:load"] )
    site_result$site.int.lo = CIs["tempt:load",1]
    site_result$site.int.hi = CIs["tempt:load",2]
    
    #### Add Site-Specific Main Effect and CI ####
    site_result$site.main.est = coef(m)["tempt"]
    site_result$site.main.pval = summary(m)$coefficients["tempt", "Pr(>|t|)"]
    site_result$site.main.SE = sqrt( vcov(m)["tempt", "tempt"] )
    site_result$site.main.lo = CIs["tempt",1]
    site_result$site.main.hi = CIs["tempt",2]
    
    #### Add Load-Stratified Analyses As in Original Study ####
    site_result$pval.load = t.test( lkl ~ tempt, data = d[ d$load == 1, ] )$p.value
    site_result$pval.no.load = t.test( lkl ~ tempt, data = d[ d$load == 0, ] )$p.value
    site_result$d.load = site_cohen( .d = d, .load = 1 )
    site_result$d.no.load = site_cohen( .d = d, .load = 0 )
    
    #### Hedges' g for Interaction ####
    site_result$g.int = site_hedges( .d = d )$g
    site_result$g.int.se = site_hedges( .d = d )$se.g
    
    #### Site-Specific Sample Sizes ####
    site_result$site.n = nrow(d)
    
    # number excluded due to being bad subjects (rather than an empty row)
    # this has to stay in site-level prep due to some manual exclusions (reported)
    # due to data collection errors
    site_result$site.n.excl = d$n.excl[1]
    
    return(site_result)
  }) %>%
  arrange(group, desc(site.n))

############################# OVERALL PROTOCOL EFFECT #############################






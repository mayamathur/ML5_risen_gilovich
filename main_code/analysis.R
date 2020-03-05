
############################# PRELIMINARIES ##############################

# load all packages
library(tidyverse)
library(papaja)
library(knitr)
library(lme4)
library(stargazer)
library(metafor)
library(rmeta)
library(lmerTest)
library(Replicate)
library(effsize)

rm(list=ls())


path.root = "~/Dropbox/Personal computer/Independent studies/Many Labs 5 (ML5)/ML5_risen_gilovich_git"
setwd(path.root)
source( "main_code/helper.R" )

# subject-level dataset
b <- read_csv("data/full_prepped_dataset.csv")
dim(b)  # should be 4441

# summarize subject-level data by site
sites <- b %>%
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


# make data dictionary for analysis dataset
# only needs to be run once in lifetime
# build dictionary from first row of analysis dataset

# dict = b[1,]
# dict$id = "Subject ID within site; starts at 1 for each site"
# dict$site = "Acronym for university or MTurk site"
# dict$load = "Indicator for whether subject was under cognitive load (1) or not (0)"
# dict$tempt = "Indicator for whether subject tempted fate (1) or not (0)"
# dict$group = "Factor for whether site was MTurk (ref), similar, or dissimilar"
# dict$is.mturk = "Indicator for whether site was MTurk (1) or any university, with similar or dissimilar collapsed (0)"
# dict$had.read = "Indicator for whether subject imagined having read (1) or not having read the material (0)"
# dict$lkl = "Numeric for perceived likelihood"
# dict$eff.split = "Numeric for effort split between cognitive load task and reading (as in original study)"
# dict$count.eff = "Numeric for amount of effort required for cognitive load task (not in original study; was for possible secondary analyses)"
# dict$count.hard = "Numeric for difficulty of cognitive load task (not in original study; was for possible secondary analyses)"
# dict$badness = "Numeric for perceived badness of not knowing answer when called on in class (not in original study; was for possible secondary analyses)"
# dict$importance = "Numeric for perceived importance of answering correctly in class (not in original study; was for possible secondary analyses)"
# dict$end.num = "Numeric for the integer on which subject stopped counting"
# dict$excluded = "Indicator for whether subject was excluded from analysis; always = 0 in analysis dataset"
# dict$n.excl = "Numeric for number of subjects excluded in this site"
# dict$SAT = "Numeric for median SAT score of site (NA if foreign or MTurk)"
# dict$pval.load = "P-value for t-test of tempt effect on lkl within load = 1 subjects"
# dict$pval.no.load = "P-value for t-test of tempt effect on lkl within load = 0 subjects"
# dict$d.load = "Cohen's d for tempt effect on lkl within load = 1 subjects"
# dict$d.no.load = "Cohen's d for tempt effect on lkl within load = 0 subjects"
# dict$g.int = "Hedges' g for tempt*load interaction within site"
# dict$g.int.se = "SE of Hedges' g for tempt*load interaction within site"
# 
# # reverted to write.csv because write_csv outputs incorrectly
# write.csv( t( dict[ , -c(1:2) ] ), "data/analysis_data_dictionary.csv")


############################# TABLE 1 AND OTHER DATA QUALITY MEASURES ##############################

# show total and excluded bad subjects as in Table 1
data.frame( site = sites$site, n.excl = sites$site.n.excl, n.total = sites$site.n)

# sample sizes by site type
table( b$group )

# total exclusions
sum( sites$site.n.excl )

# overall proportion of data that was excluded
perc.excl.total = sum( sites$site.n.excl ) / sum( sites$site.n.excl, sites$site.n )
perc.excl.MTurk = sites$site.n.excl[ sites$site == "MTurk" ] / sum( sites$site.n.excl[ sites$site == "MTurk" ], sites$site.n[ sites$site == "MTurk" ] )
perc.excl.sim = sum( sites$site.n.excl[ sites$group == "b.similar" ] ) / sum( sites$site.n.excl[ sites$group == "b.similar" ], sites$site.n[ sites$group == "b.similar" ] )
perc.excl.dis = sum( sites$site.n.excl[ sites$group == "c.dissimilar" ] ) / sum( sites$site.n.excl[ sites$group == "c.dissimilar" ], sites$site.n[ sites$group == "c.dissimilar" ] )


# means and SDs of likelihood by site type
agg.means = aggregate( lkl ~ tempt + load + group, b, mean)
agg.sds = aggregate( lkl ~ tempt + load + group, b, sd)
agg = data.frame( cbind( agg.means, agg.sds$lkl) )
names(agg)[4:5] = c("mean", "SD")
agg


############################# ORIGINAL STUDY EFFECT ESTIMATES ##############################

##### Main effect from original #####
# stats from page 302
orig.cell.n = 30
cells = 4  # 2 X 2 factorial
orig.total.n = orig.cell.n * cells
main.n = orig.cell.n * (cells/2)  # main effect conditional on tempt = 0; uses half the subjects because balanced

# raw mean difference and its variance
yi.orig.main = 2.93 - 1.90  # raw mean difference
var.mean0 = 1.42^2 / orig.cell.n
var.mean1 = 2.16^2 / orig.cell.n
vyi.orig.main = var.mean0 + var.mean1 # cells are independent

# stats on raw scale
t.crit = qt( 0.975, df = main.n - (cells/2)  )
pval.orig.main = 2 * ( 1 - pt( q = abs( yi.orig.main / sqrt(vyi.orig.main) ), df = main.n - (cells/2) ) )
orig.main.lo = yi.orig.main - t.crit * sqrt(vyi.orig.main)
orig.main.hi = yi.orig.main + t.crit * sqrt(vyi.orig.main)

# stats on SMD scale
SD.pool.main = sqrt( (1.42^2 + 2.16^2) / 2 )  # because equal sample sizes per cell
( SMD.orig.main = yi.orig.main / SD.pool.main ) # reported: 0.58; similar :)
( SE.SMD.orig.main = sqrt(vyi.orig.main) * (1/SD.pool.main) )  # treat SD as known
( CI.SMD.orig.main = SMD.orig.main + c(-1,1) * SE.SMD.orig.main * t.crit )
# sanity check:
# yi.orig.main / sqrt( vyi.orig.main )
# matches their t = 2.19 (pg 302, column 2)



##### Interaction effect from original #####
# interaction is the "difference in differences"
yi.orig.int = ( 5.27 - 2.70 ) - ( 2.93 - 1.90 )
vyi.orig.int = ( 1.42^2 / orig.cell.n ) + ( 2.16^2 / orig.cell.n ) + ( 2.17^2 / orig.cell.n ) + ( 2.36^2 / orig.cell.n )
# just add the variances that contribute to the linear combo by independence

# sanity check: reproduce original paper's F-stat
SD.pool.int = sqrt( (1.42^2 + 2.16^2 + 2.36^2 + 2.17^2) / 4 )
( SMD.orig.int = yi.orig.int / SD.pool.int ) # their reported 1.15 is NOT for the interaction, but for main effect within load = 1
( SE.SMD.orig.int = sqrt( vyi.orig.int ) * (1/SD.pool.int) )
( CI.SMD.orig.int = SMD.orig.int + c(-1,1) * SE.SMD.orig.int * qt(p = 0.975, df = orig.total.n - cells) )
# sanity check:
# F.stat = ( yi.orig.int^2 / vyi.orig.int )^2  # square a t-stat to get F-stat
# appears within rounding error (reported: F = 4.15)

# stats for interaction on raw scale
pval.orig.int = 2 * ( 1 - pt( q = abs( SMD.orig.int ), df = orig.total.n - cells ) )
orig.int.lo = yi.orig.int - qt( 0.975, df = orig.total.n - cells ) * sqrt(vyi.orig.int)
orig.int.hi = yi.orig.int + qt( 0.975, df = orig.total.n - cells ) * sqrt(vyi.orig.int)


############################# RPP REPLICATION EFFECT ESTIMATES ##############################

rpp = read.table("data/data_prepped_rpp.txt") %>%
  as_tibble %>%
  mutate(tempt = ifelse(Had.read == 1, 0, 1 ),
         lkl = Likelihood,  # rename for use in existing function
         load = Load )

# a priori exclusion criteria:
#  exclude S who ended on too-high number
#  or who put 0 effort into counting task
rpp = rpp[ ( as.numeric( rpp$End.num ) <= 561 | is.na( rpp$End.num) ) & 
             ( as.numeric( rpp$Effort ) != 0 | is.na(rpp$Effort) ), ]

# reproduce RPP main model - works :)
# summary(aov(Likelihood ~ Load * tempt, data = rpp))
lm.rpp = lm(Likelihood ~ Load * tempt, data = rpp)

#dim(rpp)  # should be 226


##### Main effect in RPP #####
means = aggregate( Likelihood ~ tempt, data = rpp[ rpp$Load == 0, ], mean)
vars = aggregate( Likelihood ~ tempt, data = rpp[ rpp$Load == 0, ], var)
ns = aggregate( Likelihood ~ tempt, data = rpp[ rpp$Load == 0, ], length)

yi.rpp.main = ( means[2,2] - means[1,2] )
vyi.rpp.main = ( vars[2,2] / ns[2,2] ) + ( vars[1,2] / ns[1,2] )

rpp.main.lo = yi.rpp.main - qt( 0.975, df = nrow(rpp[rpp$Load == 0,]) - 2 ) * sqrt( vyi.rpp.main )
rpp.main.hi = yi.rpp.main + qt( 0.975, df = nrow(rpp[rpp$Load == 0,]) - 2 ) * sqrt( vyi.rpp.main )
# indeed, this closely agrees with confint(lm.rpp) :) 

pval.rpp.main = 2 * ( 1 - pt( yi.rpp.main / sqrt( vyi.rpp.main ), df = nrow(rpp[rpp$Load == 0,]) - 2 ) )
# indeed, this closely agrees with summary(lm.rpp) :) 


##### Interaction in RPP #####
means = aggregate( Likelihood ~ tempt * Load, data = rpp, mean)
vars = aggregate( Likelihood ~ tempt * Load, data = rpp, var)
ns = aggregate( Likelihood ~ tempt * Load, data = rpp, length)

# effect of tempt vs. no-tempt for load vs. for no-load
yi.rpp.int = ( means[4,3] - means[3,3] ) - ( means[2,3] - means[1,3] ) 
vyi.rpp.int = sum( vars[,3] / ns[,3] )   # add all the cells

rpp.int.lo = yi.rpp.int - qt( 0.975, df = nrow(rpp) - 4 ) * sqrt( vyi.rpp.int )
rpp.int.hi = yi.rpp.int + qt( 0.975, df = nrow(rpp) - 4 ) * sqrt( vyi.rpp.int )
# indeed, this closely agrees with confint(lm.rpp) :) 

pval.rpp.int = 2 * ( 1 - pt( yi.rpp.int / sqrt( vyi.rpp.int ), df = nrow(rpp) - 4 ) )
# indeed, this closely agrees with summary(lm.rpp) :) 


##### SMDs in RPP #####
site_hedges(rpp, effect = "main")
site_hedges(rpp, effect = "interaction")

############################# FIT ANALYSIS MODELS 1 AND 1' ##############################

######## Primary Model 1: Only Similar Sites ######## 
# fit Primary Model 1, to be reported in subsequent section
# reference level for group is MTurk
# use ML instead of REML because of convergence troubles
m1 = lmer( lkl ~ tempt * load * group + (tempt * load | site), data = b[ b$group != "c.dissimilar", ],
           REML = FALSE )

# mystery: changing order of variables in random slope specification
#  results in convergence failure:
# lmer( lkl ~ tempt * load * group + (load * tempt | site), data = b[ b$group != "c.dissimilar", ] )

# pooled estimate and CI of main effect (similar sites)
main.sim = lin_combo( "tempt", "tempt:groupb.similar", m1 )

# pooled estimate and CI of interaction (similar sites)
int.sim = lin_combo( "tempt:load", "tempt:load:groupb.similar", m1 )

# note that p-values directly from m1 differ from those in manuscript
#  the latter use the z-dist, which is more appropriate here,
#  but the former use the t-dist.
# p-values as reported in manuscript:
z_pvals(m1)



######## Forest Plot Model: Combining All Universities ######## 

# this is different from preregistered model 1' in that it treats all 
#  universities as 1 category

# here, reference level is all university sites
m2 = lmer( lkl ~ tempt * load * is.mturk + (tempt * load | site), data = b )

# pooled estimate and CI of main effect (all universities)
CI2 = confint(m2, method = "Wald")
main.uni = data.frame( est = fixef(m2)["tempt"], lo = CI2[ "tempt", 1 ],
                       hi = CI2[ "tempt", 2 ] )


# pooled estimate and CI of interaction (all universities)
int.uni = data.frame( est = fixef(m2)["tempt:load"], lo = CI2[ "tempt:load", 1 ],
                      hi = CI2[ "tempt:load", 2 ] )

# main effect in MTurk
mturk.main.m2 = lin_combo( "tempt:is.mturk", "tempt", m2 )

# interaction effect in MTurk
mturk.int.m2 = lin_combo( "tempt:load:is.mturk", "tempt:load", m2 )

# z-based p-values
z_pvals(m2)


######## Secondary Model 1': Similar Vs. Dissimlar Vs. MTurk ######## 

# here, reference level is all university sites
m3 = lmer( lkl ~ tempt * load * group + (tempt * load | site), data = b )

# pooled estimate and CI of main effect (MTurk)
CI3 = confint(m3, method = "Wald")
main.mturk.m3 = data.frame( est = fixef(m3)["tempt"], lo = CI3[ "tempt", 1 ],
                       hi = CI3[ "tempt", 2 ], pval = z_pvals(m3)[ "tempt", ] )

# pooled estimate and CI of interaction (MTurk)
int.mturk.m3 = data.frame( est = fixef(m3)["tempt:load"], lo = CI3[ "tempt:load", 1 ],
                      hi = CI3[ "tempt:load", 2 ], pval = z_pvals(m3)[ "tempt:load", ] )

# main effect in similar sites
main.sim.m3 = lin_combo( "tempt:groupb.similar", "tempt", m3 )

# interaction effect in similar sites
int.sim.m3 = lin_combo( "tempt:load:groupb.similar", "tempt:load", m3 )

# main effect in dissimilar sites
main.dis.m3 = lin_combo( "tempt:groupc.dissimilar", "tempt", m3 )

# interaction effect in dissimilar sites
int.dis.m3 = lin_combo( "tempt:load:groupc.dissimilar", "tempt:load", m3 )

# effect of similar vs. dissimilar on main effect
main.contrast.m3 = lin_combo( "tempt:groupb.similar", "tempt:groupc.dissimilar", m3, difference = TRUE )

# effect of similar vs. dissimilar on interaction
int.contrast.m3 = lin_combo( "tempt:load:groupb.similar", "tempt:load:groupc.dissimilar", m3, difference = TRUE )

# z-based p-values
z_pvals(m3)




############################# FOREST PLOTS ##############################

library(ggplot2)
library(gridExtra)
library(grid)


##### Format Sites Dataframe For LHS of Forest Plot ##### 

temp = sites[ , names(sites) %in% c("site", "site.n", "site.main.est", "site.main.lo", "site.main.hi", "site.main.pval",
                                    "site.int.est", "site.int.lo", "site.int.hi", "site.int.pval", "is.mturk", "group", "site.n") ]

# add row for original study
temp = rbind( data.frame( site = "Original study", site.n = 120, site.main.est = yi.orig.main, site.main.lo = orig.main.lo, site.main.hi =  orig.main.hi, site.main.pval = pval.orig.main,
                          site.int.est = yi.orig.int, site.int.lo = orig.int.lo, site.int.hi = orig.int.hi, site.int.pval = pval.orig.int,
                          is.mturk = FALSE, group = "d.original"), temp )

temp$site = as.character(temp$site)

# insert header rows to be printed in bold
temp = insert_at( row = c( "RPP protocol", rep( NA, ncol(temp)-1 ) ),
                  df = temp,
                  position = 2
)

new.row = c( "Similar sites", rep( NA, ncol(temp)-1 ) )
new.row[ names(temp) == "site.main.est" ] = main.sim$est
new.row[ names(temp) == "site.main.lo" ] = main.sim$lo
new.row[ names(temp) == "site.main.hi" ] = main.sim$hi
new.row[ names(temp) == "site.main.pval" ] = main.sim$pval
new.row[ names(temp) == "site.int.est" ] = int.sim$est
new.row[ names(temp) == "site.int.lo" ] = int.sim$lo
new.row[ names(temp) == "site.int.hi" ] = int.sim$hi
new.row[ names(temp) == "site.int.pval" ] = int.sim$pval
new.row[ names(temp) == "site.n" ] = sum( sites$site.n[ sites$group == "b.similar" ] )
temp = insert_at( row = new.row, df = temp, position = 5 )


new.row = c( "Dissimilar sites", rep( NA, ncol(temp)-1 ) )
new.row[ names(temp) == "site.main.est" ] = main.dis.m3$est
new.row[ names(temp) == "site.main.lo" ] = main.dis.m3$lo
new.row[ names(temp) == "site.main.hi" ] = main.dis.m3$hi
new.row[ names(temp) == "site.main.pval" ] = main.dis.m3$pval
new.row[ names(temp) == "site.int.est" ] = int.dis.m3$est
new.row[ names(temp) == "site.int.lo" ] = int.dis.m3$lo
new.row[ names(temp) == "site.int.hi" ] = int.dis.m3$hi
new.row[ names(temp) == "site.int.pval" ] = int.dis.m3$pval
new.row[ names(temp) == "site.n" ] = sum( sites$site.n[ sites$group == "c.dissimilar" ] )
temp = insert_at( row = new.row, df = temp, position = 11 )

# cast stuff as numeric
temp$site.main.est = as.numeric(temp$site.main.est)
temp$site.main.lo = as.numeric(temp$site.main.lo)
temp$site.main.hi = as.numeric(temp$site.main.hi)
temp$site.main.pval = as.numeric(temp$site.main.pval)

temp$site.int.est = as.numeric(temp$site.int.est)
temp$site.int.lo = as.numeric(temp$site.int.lo)
temp$site.int.hi = as.numeric(temp$site.int.hi)
temp$site.int.pval = as.numeric(temp$site.int.pval)

# build plot-friendly string with point estimate and CI
temp$string.main = paste( my_round( temp$site.main.est, 2 ), " [", my_round( temp$site.main.lo, 2 ), ", ", my_round( temp$site.main.hi, 2 ), "]", sep="" )
temp$string.int = paste( my_round( temp$site.int.est, 2 ), " [", my_round( temp$site.int.lo, 2 ), ", ", my_round( temp$site.int.hi, 2 ), "]", sep="" )
temp$site.main.pval = my_round( temp$site.main.pval, 2 )
temp$site.int.pval = my_round( temp$site.int.pval, 2 )

# this avoids printing "NA" as a string on the plot
temp$string.main[ is.na(temp$site) | temp$site == "RPP protocol" ] = NA
temp$site.main.pval[ is.na(temp$site) | temp$site == "RPP protocol" ] = NA
temp$string.int[ is.na(temp$site) | temp$site == "RPP protocol" ] = NA
temp$site.int.pval[ is.na(temp$site) | temp$site == "RPP protocol" ] = NA


# add row to appear at the very top with column headers
label.row = rep(NA, ncol(temp))
label.row[ names(temp) == "site.n"] = "Analyzed n"
label.row[ names(temp) == "string.main"] = "Estimate and 95% CI"
label.row[ names(temp) == "string.int"] = "Estimate and 95% CI"
label.row[ names(temp) == "site.int.pval"] = "p-value"
temp = insert_at( row = label.row,
                  df = temp,
                  position = 1 )

# set a y-axis value for each label
# ensures alignment between LHS and RHS of forest plot
temp$yval = rev( seq( 1, nrow(temp), by = 1) )

# add spaces for indentation
temp$site[ ! temp$site %in% c("Original study", "RPP protocol", "Similar sites", "Dissimilar sites") & ! is.na(temp$site) ] = paste("   ", temp$site[ ! temp$site %in% c("Original study", "RPP protocol", "Similar sites", "Dissimilar sites") & ! is.na(temp$site) ], sep="")

# for use in plot legend (colors, plot shapes)
temp$is.pooled = ifelse( temp$site %in% c("Similar sites", "Dissimilar sites"), TRUE, FALSE )

# these functions save the forest plots to file.names
awesome_forest( data=temp, est.name="site.main.est",
                lo.name="site.main.lo", hi.name = "site.main.hi",
                file.name = "main_forest",
                site.name = "site", 
                n.name = "site.n",
                string.name = "string.main",
                pval.name = "site.main.pval",
                xlab = "Main effect estimate",
                min.x = -2.5, max.x = 2.5 )


awesome_forest( data=temp, est.name="site.int.est",
                lo.name="site.int.lo", hi.name = "site.int.hi",
                file.name = "int_forest",
                site.name = "site", 
                n.name = "site.n",
                string.name = "string.int",
                pval.name = "site.int.pval",
                xlab = "Interaction effect estimate",
                min.x = -3.5, max.x = 3.5 )



######## Parameter Estimates from Model 1 ######## 

# using Wald CIs because profile and boot are struggling to 
#  converge (i.e., assume coefficient estimates are normal,
#  which is quite reasonable at these sample sizes)

# these will exactly match the z-dist p-values that are reported throughout manuscript
CI = confint(m1, method="Wald")

# make table
name = c("Tempt main effect within MTurk", 
          "Tempt main effect within similar sites",
          "Effect of similar site vs. MTurk on tempt main effect", 
          "Tempt-load interaction within MTurk", 
          "Tempt-load interaction within similar sites",
          "Effect of similar site vs. MTurk on tempt-load interaction"
          )

value = as.numeric( c( fixef(m1)["tempt"], 
           main.sim$est, 
           fixef(m1)["tempt:groupb.similar"],   
           fixef(m1)["tempt:load"], 
           int.sim$est, 
           fixef(m1)["tempt:load:groupb.similar"]
           ) )
value = my_round(value, 2)

lo = as.numeric( c( CI["tempt", 1], 
           main.sim$lo, 
           CI["tempt:groupb.similar", 1],
           CI["tempt:load", 1], 
           int.sim$lo, 
           CI["tempt:load:groupb.similar", 1]
           ) )
lo = my_round(lo, 2)

hi = as.numeric( c( CI["tempt", 2], 
           main.sim$hi, 
           CI["tempt:groupb.similar", 2],
           CI["tempt:load", 2], 
           int.sim$hi, 
           CI["tempt:load:groupb.similar", 2]
           ) )
hi = my_round(hi, 2)

CI.string = paste( "[", lo, ", ", hi, "]", sep="" )

pvals.m1 = z_pvals(m1)
pval = as.numeric( c( pvals.m1["tempt",], 
           main.sim$pval, 
           pvals.m1["tempt:groupb.similar",],
           pvals.m1["tempt:load",], 
           int.sim$pval, 
           pvals.m1["tempt:load:groupb.similar",]
           ) )
pval = my_round(pval, 2)

m1.res = data.frame( "Name"=name, "Estimate"=value, "CI"=CI.string, "pval"=pval )



######## Table 2: Parameter Estimates from Model 1' ######## 

name = c("Tempt main effect within MTurk", 
         "Tempt main effect within similar sites",
         "Tempt main effect within dissimilar sites",
         "Effect of similar vs. dissimilar site on tempt main effect", 
         "Tempt-load interaction within MTurk", 
         "Tempt-load interaction within similar sites",
         "Tempt-load interaction within dissimilar sites",
         "Effect of similar vs. dissimilar site on tempt-load interaction"
)

value = as.numeric( c( main.mturk.m3$est, 
                       main.sim.m3$est, 
                       main.dis.m3$est, 
                       main.contrast.m3$est,  
                       int.mturk.m3$est, 
                       int.sim.m3$est, 
                       int.dis.m3$est, 
                       int.contrast.m3$est
) )
value = my_round(value, 2)

# negative ones also use the second CI limit in order to 
#  report them as "lo, hi" instead of vice-versa
lo = as.numeric( c( main.mturk.m3$lo, 
                    main.sim.m3$lo, 
                    main.dis.m3$lo, 
                    main.contrast.m3$lo,  
                    int.mturk.m3$lo, 
                    int.sim.m3$lo, 
                    int.dis.m3$lo, 
                    int.contrast.m3$lo
) )
lo = my_round(lo, 2)

hi = as.numeric( c( main.mturk.m3$hi, 
                    main.sim.m3$hi, 
                    main.dis.m3$hi, 
                    main.contrast.m3$hi,  
                    int.mturk.m3$hi, 
                    int.sim.m3$hi, 
                    int.dis.m3$hi, 
                    int.contrast.m3$hi
) )
hi = my_round(hi, 2)

CI.string = paste( "[", lo, ", ", hi, "]", sep="" )

pval = as.numeric( c( main.mturk.m3$pval, 
                      main.sim.m3$pval, 
                      main.dis.m3$pval, 
                      main.contrast.m3$pval,  
                      int.mturk.m3$pval, 
                      int.sim.m3$pval, 
                      int.dis.m3$pval, 
                      int.contrast.m3$pval
) )
pval = my_round(pval, 2)

m3.res = data.frame( "Name"=name, "Estimate"=value, "CI"=CI.string, "pval"=pval )


apa_table(m3.res, 
          col.names = c("Parameter", "Estimate", "95\\% CI", "p-value"),
          caption = "Table 3: In units of perceived likelihood on a 0-10 scale, estimates of the main effect and target interaction effect in similar university sites, dissimilar university sites, and under the RPP protocol (MTurk), as well as estimates of the difference between these estimates. Total n = 4441.")




############################# STATISTICAL CONSISTENCY ##############################

######## P_orig For Main Effect (Similar Sites) ######## 

# refit mixed model among only similar sites to get heterogeneity estimate within only these sites
m.sim = lmer( lkl ~ tempt * load + (tempt * load | site), data = b[ b$group == "b.similar", ] )
Vhat.main = VarCorr(m.sim)$site["tempt", "tempt"]
Mhat.main = fixef(m.sim)[["tempt"]]
SE.Mhat = sqrt(vcov(m.sim)["tempt", "tempt"])

( p.orig.main.sim = p_orig( orig.y = yi.orig.main, orig.vy = vyi.orig.main,
                      yr = Mhat.main, t2 = Vhat.main, vyr = SE.Mhat^2 ) )

# manual sanity check - yes :) 
# 2 * ( 1 - pnorm( abs( yi.orig.main - Mhat.main ) / sqrt( Vhat.main + vyi.orig.main + SE.Mhat^2 ) ) )

# note that the diagnostic plots use the meta-analytic MLE for heterogeneity rather than that of mixed model
#  since not sure we have a closed-form expression for the latter
plots = diag_plots( yi = sites$site.main.est[ sites$group == "b.similar" ],
                    vi = sites$site.main.SE[ sites$group == "b.similar" ]^2,
                    yi.orig = yi.orig.main,
                    vi.orig = vyi.orig.main)


######## P_orig For Interaction Effect (Similar Sites) ######## 

Vhat.int = VarCorr(m.sim)$site["tempt:load", "tempt:load"]  # variance of random slopes of tempt:load
Mhat.int = fixef(m.sim)[["tempt:load"]]
SE.Mhat = sqrt(vcov(m.sim)["tempt:load", "tempt:load"])

( p.orig.int.sim = p_orig( orig.y = yi.orig.int, orig.vy = vyi.orig.int,
                      yr = Mhat.int, t2 = Vhat.int, vyr = SE.Mhat^2) )

# note that the diagnostic plots use the meta-analytic MLE for heterogeneity rather than that of mixed model
#  since not sure we have a closed-form expression for the latter
plots = diag_plots( yi = sites$site.int.est[ sites$group == "b.similar" ],
                    vi = sites$site.int.SE[ sites$group == "b.similar" ]^2,
                    yi.orig = yi.orig.int,
                    vi.orig = vyi.orig.int)


######## P_orig For Main Effect (All Universities) ########

# fit mixed model excluding only MTurk
m.all = lmer( lkl ~ tempt * load + (tempt * load | site), data = b[ ! b$is.mturk, ] )
Vhat = VarCorr(m.all)$site["tempt", "tempt"]  # variance of random slopes of tempt
Mhat = fixef(m.all)[["tempt"]]
SE.Mhat = sqrt(vcov(m.all)["tempt", "tempt"])

( p.orig.main.uni = p_orig( orig.y = yi.orig.main, orig.vy = vyi.orig.main,
                      yr = Mhat, t2 = Vhat, vyr = SE.Mhat^2) )

plots = diag_plots( yi = sites$site.main.est[ sites$group != "a.mturk" ],
                    vi = sites$site.main.SE[ sites$group == "a.mturk" ]^2,
                    yi.orig = yi.orig.main,
                    vi.orig = vyi.orig.main)


######## P_orig For Main Effect (All Universities) ########

# same mixed model as above
Vhat = VarCorr(m.all)$site["tempt:load", "tempt:load"]   # variance of random slopes of tempt:load
Mhat = fixef(m.all)[["tempt:load"]]
SE.Mhat = sqrt(vcov(m.all)["tempt:load", "tempt:load"])

( p.orig.int.uni = p_orig( orig.y = yi.orig.int, orig.vy = vyi.orig.int,
                      yr = Mhat, t2 = Vhat, vyr = SE.Mhat^2) )

plots = diag_plots( yi = sites$site.int.est[ sites$group != "a.mturk" ],
                    vi = sites$site.int.SE[ sites$group == "a.mturk" ]^2,
                    yi.orig = yi.orig.int,
                    vi.orig = vyi.orig.int)


############################# SECONDARY: MECHANISMS FOR REPLICATION FAILURE ##############################

######## Efficacy of Cognitive Load on MTurk ########

# reload lmerTest - have to go back and forth
# because it caused incompatibility elsewhere
library(lmerTest)

# MTurk cannot have its own random slope because only one such site
m.manip = lmer( lkl ~ tempt * load * is.mturk + (tempt * load | site), data = b )
CI.manip = confint(m.manip, method = "Wald")

# z-based p-values
z_pvals(m.manip)


######## Effort Of Cognitive Load Task ########

# use only subjects actually assigned to cognitive load
# mturk cannot have its own random slope because only one such site
m.effort = lmer( count.eff ~ is.mturk + (1 | site), data = b[ b$load==1, ] )
CI.effort = confint(m.effort, method = "Wald")

# z-based p-values
z_pvals(m.effort)

######## Perceived Importance of Answering Questions Correctly ########
m.import = lmer( importance ~ group + (1 | site), data = b )
CI.import = confint(m.import, method = "Wald")

# z-based p-values
z_pvals(m.import)

######## Perceived Negativity of Answering Questions Correctly ########
m.bad = lmer( badness ~ group + (1 | site), data = b )
CI.bad = confint(m.bad, method = "Wald")

# z-based p-values
z_pvals(m.bad)


######## Difficulty Of Cognitive Load Task ########
# use only subjects actually assigned to cognitive load
m.hard = lmer( count.hard ~ is.mturk + (1 | site), data = b[ b$load==1, ] )
CI.hard = confint(m.hard, method = "Wald")

# z-based p-values
z_pvals(m.hard)


######## Moderation Of Target Interaction By SAT Score ########

# center and scale SAT score for interpretability
b$SATc = ( b$SAT - mean(b$SAT, na.rm=TRUE) ) / 10

m.sat = lmer( lkl ~ tempt * load * SATc + (tempt * load | site), data = b )
CI.SAT = confint(m.sat, method = "Wald")




############################# STATS FOR CHARLIE'S OVERARCHING MANUSCRIPT ##############################

protocol.rmd = fixef(m1)["tempt:load:groupb.similar"]
protocol.rmd.se = sqrt( vcov(m1)["tempt:load:groupb.similar", "tempt:load:groupb.similar"] )

# residual (aka conditional) variance
# will be very close to the marginal SD(b$lkl) since basically no non-null effects in the model
sd.pool = sigma(m1)

protocol.d = protocol.rmd / sd.pool
# treats sd.pool as a known constant, but with this sample size, this will barely matter
protocol.d.se = sqrt( (1/sd.pool)^2 * protocol.rmd.se^2 )


############################# SAVE OBJECTS ##############################

# save R objects for Markdown manuscript
save.image( file = "analysis_objects.rds" )

# site-specific dataset
setwd(path.root)
setwd("data/for_overarching")
write.csv(sites, "site_summary_stats.csv", row.names = FALSE)

# overall Risen & Gilovich protocol effect for Charlie
write.csv( data.frame( protocol.d,
                       protocol.d.se ),
           "overall_protocol_estimate.csv",
           row.names = FALSE )



############################# HELPER FNS ##############################

# mod: the model 
# var: quoted name of variable on which to conduct inference
gee_output = function( mod, var ) {
  crit = qnorm(.975)
  est = as.numeric( coef(mod)[var] )
  
  # robust SEs
  se = as.numeric( sqrt( mod$robust.variance[var, var] ) )
  lo = as.numeric( est - crit * se )
  hi = as.numeric( est + crit * se )
  pval = as.numeric( 2 * ( 1 - pnorm( abs( est ) / se ) ) )
  
  output = list(est=est, se=se, lo=lo, hi=hi, pval=pval)
  print(output)
  return( output )
}


lmm_output = function( mod, var ) {
  crit = qnorm(.975)
  est = as.numeric( fixef(mod)[var] )
  se = as.numeric( sqrt( vcov(mod)[var, var] ) )
  lo = as.numeric( est - crit * se )
  hi = as.numeric( est + crit * se )
  pval = as.numeric( 2 * ( 1 - pnorm( abs( est ) / se ) ) )
  
  output = list(est=est, se=se, lo=lo, hi=hi, pval=pval)
  print(output)
  return( output )
}

notNA = function(x) {
  x[ !is.na(x) ]
}


############################# READ IN DATA ##############################

# read in saved objects from my ML5 analyses
path.root = "~/Dropbox/Personal computer/Independent studies/Many Labs 5 (ML5)/ML5_risen_gilovich_git"
setwd(path.root)
load( "main_code/analysis_objects.rds" )

# ~~~ CHANGE DATA PATH ONCE IT CAN BE PUBLICLY AVAILABLE
# all ML2 data for Slate 2 studies
setwd("~/Dropbox/Personal computer/Independent studies/Many Labs 5 (ML5)/Many Labs 2 materials")
d0 = read.csv("ML2_RawData_S2 (1).csv")

# site-level t-tests for all sites
setwd("~/Dropbox/Personal computer/Independent studies/Many Labs 5 (ML5)/ML5_risen_gilovich_git/compare_to_ml2/prepped_datasets")
agg = read.csv( "ML2_all_site_summary.csv", header = TRUE )

# site-level t-tests for all sites
agg.u = read.csv( "ML2_undergrads_site_summary.csv", header = TRUE )

# long data from both ML2 and ML5 (with all sites)
bl = read.csv( "both_long_data.csv", header = TRUE )

# short data with OLS estimates from both ML2 and ML5 (with all sites)
bs = read.csv( "both_short_data.csv", header = TRUE )

# undergrads from ML2
u = read.csv( "ML2_undergrad_data.csv", header = TRUE )


############################# META-ANALYZE ML2 ##############################

library(metafor)
ES = escalc( measure = "SMD", m2i = agg.u$m0, m1i = agg.u$m1,
             sd2i = agg.u$sd0, sd1i = agg.u$sd1,
             n2i = agg.u$n0, n1i = agg.u$n1)
m = rma.uni( ES, method="REML", knha = TRUE)
summary(m)
# very similar to naive Cohen's d on undergrads because no heterogeneity


####### Forest Plot (Not in Paper) ####### 
# for plotting joy
agg.u$yval = 1:nrow(agg.u)

# color-code the elite schools
agg.u$is.pooled = FALSE
agg.u$is.pooled[ agg.u$source %in% c("yale", "umich", "carleton", "kenyon") ] = TRUE

# build plot-friendly string with point estimate and CI
agg.u$string = paste( round( agg.u$site.main, 2 ),
                      " [", round( agg.u$lo, 2 ), ", ",
                      round( agg.u$hi, 2 ), "]", sep="" )
agg.u$pval = round( agg.u$pval, 2)

setwd("~/Dropbox/Personal computer/Independent studies/Many Labs 5 (ML5)/ML5_risen_gilovich/compare_to_ml2")
source("awesome_forest_plot_v2.R")
awesome_forest( data = agg.u,
                est.name = "site.main",
                lo.name = "lo",
                hi.name = "hi",
                site.name = "source",
                file.name = "ml2_undergrads_forest.png",
                n.name = "N",
                string.name = "string",
                pval.name = "pval",
                xlab = "Mean difference", 
                min.x = -1.5,
                max.x = 2.5 )



############################# COMPARE ML2 RESULTS ON U.S. MTURKERS TO OURS ##############################

# ML2
ml2.turk.est = agg$site.main[ agg$source=="mturk" ]
ml2.turk.n = agg$N[ agg$source=="mturk" ]
agg$pval[ agg$source=="mturk" ]

# ML5
ml5.turk.est = sites$site.main.est[ sites$site == "MTurk" ]
ml5.turk.n = sites$site.n[ sites$site == "MTurk" ]
sites$site.main.pval[ sites$site == "MTurk" ]

# point estimates almost exactly the same
# obviously p-value is way smaller for ML5 because of larger sample size



############################# COMPARE ALL OUR SITES TO ALL OF THEIRS ##############################

# nearly identical results for LMM vs. GEE

######## With Mixed Model on IPD ######## 

library(lme4)

lmm1 = lmer( lkl ~ tempt * study + (1|site) + (tempt|site), data=bl )
summary(lmm1)

# ignore study for overall estimate
lmm2 = lmer( lkl ~ tempt + (1|site) + (tempt|site), data=bl )
summary(lmm2)


######## With GEE on IPD ######## 

# put in a study covariate
# working independence
library(gee)
gee1 = gee( lkl ~ tempt * study, id=site, data=bl,
                scale.fix=FALSE, corstr="independence" )
summary(gee1)


# ignore study for overall estimate
gee2 = gee(lkl ~ tempt, id=site, data=bl,
                scale.fix=FALSE, corstr="independence")
summary(gee2)


######## With Test Stats Only ######## 

# ML5 main analysis sites
res = t.test( lkl ~ (tempt == 0), data = bl[ bl$study == "ML5", ] )
t5 = res$statistic
diff5 = res$estimate[1] - res$estimate[2]
hi5 = res$conf.int[2]
df5 = res$parameter
# back-calculate the SE from the test stat and CI limit
se5 = as.numeric( (hi5 - diff5) / qt( .975, df = df5 ) ) # denom will be very close to 1.96
diff5/se5; t5  # sanity check
n5 = sum(bl2$study == "ML5")

# ML2 main analysis sites
res = t.test( lkl ~ (tempt == 0), data = bl[ bl$study == "ML2", ] )
t2 = res$statistic
diff2 = res$estimate[1] - res$estimate[2]
hi2 = res$conf.int[2]
df2 = res$parameter
se2 = as.numeric( (hi2 - diff2) / qt( .975, df = df2 ) ) # denom will be very close to 1.96
diff2/se2; t2  # sanity check
n2 = sum(bl2$study == "ML2")

# difference in differences
se.diff = sqrt( se2^2 + se5^2 )
z = abs( diff5 - diff2 ) / se.diff


######## Cohen's d Combining All Sites ######## 
d.all = cohen.d( bl$lkl ~ bl$tempt )
# 0.17 and significant


######## Save Objects for Manuscript ######## 

gee.all.diff = gee_output(gee1, "tempt:studyML5")
lmm.all.diff = lmm_output(lmm1, "tempt:studyML5")

gee.all.overall = gee_output(gee2, "tempt")
lmm.all.overall = lmm_output(lmm2, "tempt")

pval.all.stats = 2 * ( 1 - pnorm(z) )
lo.all.stats = (diff5 - diff2) - qnorm(.975) * se.diff
hi.all.stats = (diff5 - diff2) + qnorm(.975) * se.diff

los = round( c( lmm.all.diff$lo, gee.all.diff$lo, lo.all.stats ), 2 )
his = round( c( lmm.all.diff$hi, gee.all.diff$hi, hi.all.stats ), 2 )
pvals = my_round( c(lmm.all.diff$pval, gee.all.diff$pval, pval.all.stats), 2 )

all.table = data.frame( Name = c("LMM", "GEE", "RMD"),
                        AllowsCorr = c("Yes, with normal site effects",
                                       "Yes, without assumptions on structure",
                                       "No"),
                        AssumpHomo = c("Yes", "No", "Yes"),
                        Estimate = round( c( lmm.all.diff$est,
                                             gee.all.diff$est,
                                             diff5 - diff2 ), 2 ),
                        CI = paste( "[", los, ", ", his, "]", sep="" ),
                        pvals = pvals
)




############################# COMPARE MAIN ANALYSIS SITES FOR EACH STUDY ##############################

# keep only ML5's similar sites
similars = c("Stanford", "U Penn", "UCB", "UVA")
bl2 = bl[ bl$study == "ML2" | ( bl$study == "ML5" & bl$site %in% similars ), ]

# and ML2's undergrads
bl2 = bl2[ bl2$study == "ML5" | ( bl2$study == "ML2" & bl2$undergrads == "Yes" ), ]

# how many from each study?
table(bl2$study)
# should be 4599 and 754 (main analysis sample sizes)


######## With Mixed Model on IPD ######## 

lmm3 = lmer( lkl ~ tempt * study + (1|site) + (tempt|site), data=bl2 )
summary(lmm3)

# ignore study for overall estimate
lmm4 = lmer( lkl ~ tempt + (1|site) + (tempt|site), data=bl2 )
summary(lmm4)


######## With GEE on IPD ######## 

# put in a study covariate
# working independence
library(gee)
gee3 = gee( lkl ~ tempt * study, id=site, data=bl2,
            scale.fix=FALSE, corstr="independence" )
summary(gee3)

# ignore study for overall estimate
gee4 = gee(lkl ~ tempt, id=site, data=bl2,
           scale.fix=FALSE, corstr="independence")
summary(gee4)


######## With Test Stats Only ######## 

# ML5 main analysis sites
res = t.test( lkl ~ (tempt == 0), data = bl2[ bl2$study == "ML5", ] )
t5 = res$statistic
diff5 = res$estimate[1] - res$estimate[2]
hi5 = res$conf.int[2]
df5 = res$parameter
se5 = as.numeric( (hi5 - diff5) / qt( .975, df = df5 ) ) # denom will be very close to 1.96
diff5/se5; t5  # sanity check
n5 = sum(bl2$study == "ML5")

# ML2 main analysis sites
res = t.test( lkl ~ (tempt == 0), data = bl2[ bl2$study == "ML2", ] )
t2 = res$statistic
diff2 = res$estimate[1] - res$estimate[2]
hi2 = res$conf.int[2]
df2 = res$parameter
se2 = as.numeric( (hi2 - diff2) / qt( .975, df = df2 ) ) # denom will be very close to 1.96
diff2/se2; t2  # sanity check
n2 = sum(bl2$study == "ML2")

# difference in differences
se.diff = sqrt( se2^2 + se5^2 )
z = abs( diff5 - diff2 ) / se.diff



######## Save Objects for Manuscript ######## 

gee.sim.diff = gee_output(gee3, "tempt:studyML5")
lmm.sim.diff = lmm_output(lmm3, "tempt:studyML5")

gee.sim.overall = gee_output(gee4, "tempt")
lmm.sim.overall = lmm_output(lmm4, "tempt")

pval.sim.stats = 2 * ( 1 - pnorm(z) )
lo.sim.stats = (diff5 - diff2) - qnorm(.975) * se.diff
hi.sim.stats = (diff5 - diff2) + qnorm(.975) * se.diff

los = round( c( lmm.sim.diff$lo, gee.sim.diff$lo, lo.sim.stats ), 2 )
his = round( c( lmm.sim.diff$hi, gee.sim.diff$hi, hi.sim.stats ), 2 )
pvals = my_round( c(lmm.sim.diff$pval, gee.sim.diff$pval, pval.sim.stats), 2 )

sim.table = data.frame( Name = c("LMM", "GEE", "RMD"),
                        AllowsCorr = c("Yes, with normal site effects",
                                       "Yes, without assumptions on structure",
                                       "No"),
                        AssumpHomo = c("Yes", "No", "Yes"),
                        Estimate = round( c( lmm.sim.diff$est,
                                             gee.sim.diff$est,
                                             diff5 - diff2 ), 2 ),
                        CI = paste( "[", los, ", ", his, "]", sep="" ),
                        pvals = pvals
                        )


######## Cohen's d With Only Main Analysis Sites From Each Study ######## 
library(effsize)
d.sim = cohen.d( bl2$lkl ~ bl2$tempt )
# 0.20 and significant



############################# CONSISTENCY OF ML2 WITH ORIGINAL ##############################

# effect sizes from original study (from objects produced by analysis.R)
yi.orig.main
vyi.orig.main

# using main analysis sites from ML2

# difference in differences
se.diff = sqrt( se2^2 + vyi.orig.main )
z = abs( yi.orig.main - diff2 ) / se.diff

pval.orig.ml2.stats = 2 * ( 1 - pnorm(z) )


############################# CONSISTENCY OF OUR COMBINED ESTIMATES WITH ORIGINAL ##############################

######### All Sites ######### 
Vhat.sim = VarCorr(lmm2)$site.1["tempt", "tempt"]
Mhat.sim = fixef(lmm2)[["tempt"]]
SE.Mhat.sim = sqrt(vcov(lmm2)["tempt", "tempt"])

library(Replicate)
p.orig.agg.sim = p_orig( orig.y = yi.orig.main, orig.vy = vyi.orig.main,
                         yr = Mhat.sim, t2 = Vhat.sim, vyr = SE.Mhat.sim^2 )

# look at Normality
hist(bs$est)


######### Primary Analysis Sites ######### 
Vhat.all = VarCorr(lmm4)$site.1["tempt", "tempt"]
Mhat.all = fixef(lmm4)[["tempt"]]
SE.Mhat.all = sqrt(vcov(lmm4)["tempt", "tempt"])

library(Replicate)

p.orig.agg.all = p_orig( orig.y = yi.orig.main, orig.vy = vyi.orig.main,
                         yr = Mhat.all, t2 = Vhat.all, vyr = SE.Mhat.all^2 )


# look at Normality
# sites used in main analyses
main.sites = unique(bl2$site)
bs$main.analysis = bs$site %in% main.sites
hist(bs$est[ bs$main.analysis == 1 ])  # not great



############################# HOW MUCH MISSING DATA DID THEY HAVE? ##############################

# how many subjects skipped the study completely?
d0$skipped = is.na(d0$rise1.3) & is.na(d0$rise2.3)
perc.missing.ml2 = round( prop.table( table(d0$skipped) )["TRUE"] * 100, 1 )



############################# SIMPLIFIED FOREST PLOT WITH ALL SITES ##############################

# rearrange the both-short dataframe
dp1 = bs[ order(bs$main.analysis, bs$est, decreasing=TRUE), ]
space.factor = 15
dp1$yval = rev( seq( 1, space.factor * nrow(dp1), space.factor ) )

# sites used in main analyses
main.sites = unique(bl2$site)
dp1$main.analysis = dp1$site %in% main.sites

min.x = -3.5
max.x = 3
  
colors = c("gray", "black")
shapes = c(2,16)

plt = ggplot(dp1) + 
  theme_bw() + 
  aes( x = as.numeric( dp1$est ),
       xmin = as.numeric( dp1$lo ), xmax = as.numeric( dp1$hi ),
       y = as.numeric(yval),
       color = main.analysis,
       size = 1/(dp1$se^2), 
       shape = study ) + 
  scale_color_manual( values = colors, name = "Site in main analyses" ) +
  scale_shape_manual( values = shapes, name = "Study" ) +
  scale_size(guide="none" ) +
  geom_point() + 
  geom_errorbarh(height = 0.3, size=0.8 ) + 
  geom_vline(xintercept = 0) + 
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank()
   # legend.position = "none"
  )  +
  scale_x_continuous( limits=c(min.x, max.x), breaks=seq(min.x, max.x, .5) ) +
  scale_y_continuous( limits=c( 0, max( dp1$yval ) ), breaks=seq( 0, max( dp1$yval ), 1 ) ) +
  xlab("Main effect estimate (raw mean difference)")

ggsave( filename = "all_ml2_ml5_forest.png",
        plot=plt, path=NULL, width=13, height=13, units="in")


############################# SAVE OBJECTS FOR USE IN MANUSCRIPT ##############################

setwd(path.root)
save.image( file = "compare_to_ml2/ml2_comparison_objects.rds" )


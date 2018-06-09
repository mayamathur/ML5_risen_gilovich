

############################# READ IN ALL DATA ##############################

library(tidyr)
library(dplyr)

notNA = function(x) {
  x[ !is.na(x) ]
}


# read in saved objects from my ML5 analyses
path.root = "~/Dropbox/Personal computer/Independent studies/Many Labs 5 (ML5)/ML5_risen_gilovich/main_code"
setwd(path.root)
load( "analysis_objects.rds" )

# ~~~ CHANGE DATA PATH ONCE IT CAN BE PUBLICLY AVAILABLE
# this is all of ML2's Slate 2 studies, not just Risen & Gilovich
setwd("~/Dropbox/Personal computer/Independent studies/Many Labs 5 (ML5)/Many Labs 2 materials")
d0 = read.csv("ML2_RawData_S2 (1).csv"); dim(d0)


##### Keep Rows With Data For Risen & Gilovich ######
keepers = c( "rise1.3",
             "rise2.3",
             "Location",
             "Setting", 
             "Pencil",
             "source",
             "SubjectPool")

# only keep subjects who completed this study
d = d0[ ( ! is.na( d0$rise1.3) ) | ( ! is.na( d0$rise2.3) ), names(d0) %in% keepers ]

# based on ML2 codebook, contrast of interest is 
#  rise1.3 (tempt) vs. rise2.3 (don't tempt)
# if you have data for the former variable, then you were in tempting-fate condition
d$tempt = !is.na( d$rise1.3 )

# merge the two outcome vectors
d$lkl = d$rise1.3
d$lkl[ !is.na(d$rise2.3) ] = d$rise2.3[ !is.na(d$rise2.3) ]


# keep only undergrads for primary analyses
table(d$SubjectPool)  # should be 4599 undergrads
u = d[ d$SubjectPool == "Yes" & !is.na(d$SubjectPool), ]; nrow(u)  # should be 4,599


setwd("~/Dropbox/Personal computer/Independent studies/Many Labs 5 (ML5)/ML5_risen_gilovich/compare_to_ml2/prepped_datasets")
write.csv(u, "/compare_to_ml2/prepped_datasets/ML2_undergrad_data.csv")


# which sites did R&G?
table( d$Location )


############################# SITE-LEVEL T-TESTS ##############################


agg = d %>% group_by( source ) %>%
  summarize( undergrads = SubjectPool[1],
            m0 = mean(rise2.3, na.rm=TRUE),
             m1 = mean(rise1.3, na.rm=TRUE),
             sd0 = sd(rise2.3, na.rm=TRUE),
             sd1 = sd(rise1.3, na.rm=TRUE),
             n0 = length( notNA(rise2.3) ),
             n1 = length( notNA(rise1.3) ),
             N = length(rise2.3),  # because has NAs for the other condition, so total length is all S
             lo = t.test(rise1.3, rise2.3)$conf.int[1],
             hi = t.test(rise1.3, rise2.3)$conf.int[2],
             pval = t.test(rise1.3, rise2.3)$p.value,
             site.main = m1 - m0 )

write.csv( agg, "MM_site_summary.csv")


############################# REPRODUCE ML2's RESULTS ##############################

# reproduce their t-test using all subjects and ignoring correlation by site
# yes! exactly the same
t.test( d$rise1.3, d$rise2.3 )
library(effsize)
cohen.d( notNA(d$rise1.3), notNA(d$rise2.3) )  # 0.18 and significant

# now use only undergrads (main test)
t.test( u$rise1.3, u$rise2.3 )
library(effsize)
cohen.d( notNA(u$rise1.3), notNA(u$rise2.3) )
# yes :)

# m1 = tempt
# m0 = don't tempt
agg.u = u %>% group_by( source ) %>%
  summarize( m0 = mean(rise2.3, na.rm=TRUE),
             m1 = mean(rise1.3, na.rm=TRUE),
             sd0 = sd(rise2.3, na.rm=TRUE),
             sd1 = sd(rise1.3, na.rm=TRUE),
             n0 = length( notNA(rise2.3) ),
             n1 = length( notNA(rise1.3) ),
             N = length(rise2.3),
             lo = t.test(rise1.3, rise2.3)$conf.int[1],
             hi = t.test(rise1.3, rise2.3)$conf.int[2],
             pval = t.test(rise1.3, rise2.3)$p.value,
             # https://stats.stackexchange.com/questions/30394/how-to-perform-two-sample-t-tests-in-r-by-inputting-sample-statistics-rather-tha
             # unequal variances
             SE = sqrt( (1/n1 + 1/n0) * ( (n1-1)*sd1^2 + (n0-1)*sd0^2 ) / (n1 + n0 - 2) ),
             site.main = m1 - m0 )


setwd("~/Dropbox/Personal computer/Independent studies/Many Labs 5 (ML5)/ML5_risen_gilovich/compare_to_ml2/prepped_datasets")
write.csv( agg, "ML2_all_site_summary.csv")
write.csv( agg.u, "ML2_undergrads_site_summary.csv")



############################# BOTH-LONG DATASET (IPD FOR BOTH ML2 AND ML5) ##############################

# includes all sites for both studies
bl = data.frame( study = c( rep( "ML2", nrow(d) ), rep( "ML5", nrow(b) ) ),
                 site = c( as.character(d$source), as.character(b$site) ), 
                 lkl = c( d$lkl, b$lkl ),
                 tempt = c( d$tempt, b$tempt),
                 undergrads = c( as.character(d$SubjectPool), rep( "Yes", nrow(b) ) ) )

setwd("~/Dropbox/Personal computer/Independent studies/Many Labs 5 (ML5)/ML5_risen_gilovich/compare_to_ml2/prepped_datasets")
write.csv(bl, "both_long_data.csv")



############################# BOTH-SHORT DATASET (OLS ESTIMATES FOR BOTH ML2 AND ML5) ##############################

# for forest plot
bs = bl %>% group_by(study, site) %>% 
  summarize( est = coef( lm(lkl ~ tempt) )[ "tempt" ],
             lo = confint( lm(lkl ~ tempt) )[ "tempt", 1],
             hi = confint( lm(lkl ~ tempt) )[ "tempt", 2],
             se = sqrt( vcov( lm(lkl ~ tempt) )["tempt", "tempt"] ) )


setwd("~/Dropbox/Personal computer/Independent studies/Many Labs 5 (ML5)/ML5_risen_gilovich/compare_to_ml2/prepped_datasets")
write.csv(bs, "both_short_data.csv")



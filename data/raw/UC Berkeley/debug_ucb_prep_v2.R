


############### PREP DATA MANUALLY ############### 

library(dplyr)
source( "~/Dropbox/Personal computer/Independent studies/Many Labs 5 (ML5)/Linked to OSF/3. Analysis/R code/data_prep_functions.R" )

setwd("~/Dropbox/Personal computer/Independent studies/Many Labs 5 (ML5)/Linked to OSF/2. Data/Raw data/UC Berkeley")
raw = read.csv("raw_ucb.csv", header=TRUE)
d = raw

# merge end-number columns
# warning about "NAs introduced by coercion", but is correct
d$end.num = coalesce( as.numeric( as.character( d$Q3 ) ),
                      as.numeric( as.character( d$Q8 ) ) )

# merge effort-split columns
# warning about "NAs introduced by coercion", but is correct
d$eff.split = coalesce( as.numeric( as.character( d$Q4_1 ) ),
                        as.numeric( as.character( d$Q9_1 ) ) )

# placeholders for vars not collected
d$badness = NA
d$importance = NA
d$count.eff = NA
d$count.hard = NA

#
#write.csv(d, "manualprep_ucb.csv")
# dim(d) # 225 subjects here

d$eff.split = as.numeric(as.character(d$eff.split))
d$end.num = as.numeric(as.character(d$end.num))

# should exclude 16 for bad effort split
table(d$eff.split)

# should exclude 12 for bad end number
table( d$end.num >= 561 )

# should exclude 23 total due to overlaps in the above problems:
d$bad = ( !is.na( d$eff.split ) & d$eff.split == 0 ) | ( !is.na( d$end.num ) & d$end.num >= 561 )
table(d$bad)

# remove bad ones
d = d[ !d$bad, ]

# remove extra header row
d = d[ -1,]

dim(d)
# so total n = 201




# reproduce means and SDs
library(plyr); library(dplyr)
d$lkl = coalesce( as.numeric( as.character(d$Q2_1)),
                  as.numeric( as.character(d$Q7_1)),
                  as.numeric( as.character(d$Q11_1)),
                  as.numeric( as.character(d$Q14_1)) )
mean(d$lkl, na.rm=TRUE)
# matches my PDF :) 

# how many have NA likelihood?
table(is.na(d$lkl))
# one person




############### COMPARE TO EXISTING SCRIPT ############### 

start.path = "~/Dropbox/Personal computer/Independent studies/Many Labs 5 (ML5)/Linked to OSF/2. Data/Raw data/UC Berkeley/manualprep_ucb.csv"
end.path = "~/Dropbox/Personal computer/Independent studies/Many Labs 5 (ML5)/Linked to OSF/2. Data/Prepped data"

d2 = prep_site_data( start.path = start.path, end.path = end.path, 
                lkl.names = c("Q2_1", "Q7_1",
                              "Q11_1", "Q14_1"),
                had.read.name = "Had.read",
                load.name = "Cognitive.load",
                end.num.name = "end.num",
                eff.split.name = "eff.split",
                count.eff.name = "count.eff",
                count.hard.name = "count.hard",
                badness.name = "badness",
                importance.name = "badness",
                .site.name = "UCB", .group = "b.similar",
                .n.extra.header.rows = 1,
                .orig.id.name = "a" )

# how many have NA likelihood?
# none
table(is.na(d2$lkl))

# look for subjects who don't exist in Don's data
d2[d2$lkl == 0 & d2$end.num == 543 & d2$eff.split == 4, ]

# find in Don's original file
d2[d2$end.num == 532 & d2$lkl == 9, ]

# find them in the raw data
raw[raw$a %in% c("R_1gT8kxRv1F8fLOq", "R_1DUqCnzuWPg00BR"), ]

d2[ d2$orig.id.name == "R_1DUqCnzuWPg00BR", ]

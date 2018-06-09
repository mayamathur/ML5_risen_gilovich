rm(list=ls())
library(xlsx)

data <- read.xlsx("raw_kul.xlsx", sheetIndex = 1)

# remove superfluous rows and columns
data <- data[-c(1), ]
data <- subset(data, select=c("had.read", "load", "L1.R1.scenario_1", "L1.R0.scenario_1", "L0.R1.text_1",
                              "L0.R0.text_1", "Q29_1","Q22_1", "Q21_1","Q14_1", "Q26_1","Q28"))

# number of participants
np <- dim(data)[1]
print(np)

#create id variable
data$id <- c(1:np)

# create variable lkl by merging columns "L1.R1.scenario_1", "L1.R0.scenario_1", "L0.R1.text_1" and
# "L0.R0.text_1", and then remove these four colums
data$lkl <- rowSums(cbind(as.numeric(as.character(data[,3])), as.numeric(as.character(data[,4])),
                          as.numeric(as.character(data[,5])), as.numeric(as.character(data[,6]))), na.rm=T)
data <- data[,-c(3:6)]

# change column names
colnames(data) <- c("had.read", "load", "eff.split","count.eff", "count.hard","badness", "importance","end.num","id","lkl")

# subjects who reported ending on a number greater than or equal to 561
bad.end.num <- data$id[which(as.numeric(as.character(data$end.num))>=561)]

# subjects who reported effort split == 0
no.eff.split <- data$id[which(as.numeric(as.character(data$eff.split))==0)]

# all bad subjects
bad.subj <- cbind(bad.end.num, no.eff.split)

# remove bad subjects
data <- data[-data$id[bad.subj],]

# final n
dim(data)[1]

# marginal means and SDs
mean(as.numeric(as.character(data$load)))
sd(as.numeric(as.character(data$load)))

mean(as.numeric(as.character(data$had.read)))
sd(as.numeric(as.character(data$had.read)))

mean(as.numeric(as.character(data$lkl)))
sd(as.numeric(as.character(data$lkl)))

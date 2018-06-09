#Move "raw_upenn.csv" to desktop 
#Session 
#Set working directory
#Choose directory (select desktop)

#Open data 

data<-read.csv("raw_upenn.csv", sep = ",", na.strings = c("NA", ""), stringsAsFactor = FALSE)


#Remove unecessary rows and restore normal numbering to rows 

data<-data[c(-1,-2),]

row.names(data) <- NULL

#Remove subjects who reported ending on a number greater than or equal to 561

data<-subset(data, data$Q28<561 | is.na(data$Q28))

#Remove subjects who reported zero effort 

data<-subset(data, data$Q29_1 !=0 | is.na(data$Q29_1))

#Sample size after exclusions 

n<-as.numeric(nrow(data))

#mean and standard deviation of load 

meanload<-mean(as.numeric(data$load))

sdload<-sd(as.numeric(data$load))

#mean and standard deviation of tempt

tempt<-as.numeric(data$had.read)

meantempt<-mean(tempt)

sdtempt<-sd(tempt)

#mean and standard deviation of lkl

L1.R1.scenario_1<-as.numeric(data$L1.R1.scenario_1)

L1.R0.scenario_1<-as.numeric(data$L1.R0.scenario_1)

L0.R1.text_1<-as.numeric(data$L0.R1.text_1)

L0.R0.text_1<-as.numeric(data$L0.R0.text_1)

lklvars<-c("L1.R1.scenario_1","L1.R0.scenario_1","L0.R1.text_1", "L0.R0.text_1" )

lkltable<-data[lklvars]

lkltable$lkl=lkltable$L1.R1.scenario_1

lkltable$lkl[!is.na(lkltable$L1.R0.scenario_1)] = data$L1.R0.scenario_1[!is.na(lkltable$L1.R0.scenario_1)] 

lkltable$lkl[!is.na(lkltable$L0.R1.text_1)] = lkltable$L0.R1.text_1[!is.na(lkltable$L0.R1.text_1)] 

lkltable$lkl[!is.na(lkltable$L0.R0.text_1)] = lkltable$L0.R0.text_1[!is.na(lkltable$L0.R0.text_1)] 

lkl<-as.numeric(lkltable$lkl)

meanlkl<-mean(lkl)

sdlkl<-sd(lkl)



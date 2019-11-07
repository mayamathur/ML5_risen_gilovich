
# Contact: Maya Mathur (mmathur@stanford.edu)

############### ESTIMATE BETAS FROM RPP DATA ###############

# RPP data
setwd("~/Dropbox/Personal computer/Miscellaneous/Stanford/Junior year Stanford/Winter Quarter/Psych 254/Risen & Gilovich replication/OSF TIME CAPSULE")
d = read.csv("data_prepped_2.csv")
dim(d)  # 226 X 8 as expected

d$tempt = 0
d$tempt[d$Had.read == 0] = 1

# steal its betas and error SD
m = lm( Likelihood ~ tempt*Load, data=d )
betas = as.numeric( coef( m ) )
e = summary(m)$sigma  # in R's notation, sigma has SDs instead of vars


############### POWER SIM FOR PRIMARY ANALYSES ###############
# power simulation for primary analyses
# Te: 1/0 tempted fate vs. didn't
# L: 1/0 load vs. no load
# E: 1/0 endorsed site vs. not (MTurk)

b0 = betas[1]  # average perceived likelihood when E=0, L=0, Te=0 (saturated)
bT = betas[2]
bL = betas[3]
bE = 0  
bTL = betas[4]
bEL = 0
# the remaining two will be manipulated below


##### Fn: Simulate 1 Dataset #####
sim_data = function(.n.Turk, .n.lab, .bTE, .bTLE, .error.sd=e) {
  
  # simulate covariates
  # ignore that there are multiple labs
  E = c( rep(1, .n.lab), rep(0, .n.Turk) )
  N = length(E)
  L = rbinom( n=N, size=1, prob=0.5 )  # random assignment
  Te = rbinom( n=N, size=1, prob=0.5 )
  
  # simulate continuous outcome (perceived likelihood from 0-10) = linear predictor + Normal error
  means = c(b0 + bT*Te + bE*E + bL*L + bTL*Te*L + .bTE*Te*E + bEL*E*L + .bTLE*Te*L*E)
  length(means)
  Y = rnorm( N, mean = means, sd=.error.sd )
  return( data.frame(Y, E, L, Te) )
}


##### Fn: Compute Power for Given Parameters #####
power = function(.params, .reps=10000) {

  # initialize rejection vector
  rej=c()
  
  for( j in 1:nrow(.params) ) {  # all simulation scenarios
    for (i in 1:.reps) {
      if(i %% 1000 == 0) cat("/n/n", i)  # monitor
    
      ##### Simulate Dataset #####
      # using the appropriate row of parameters
      ds = sim_data(.n.Turk=.params$N.Turk[j], .n.lab=.params$N.Lab[j],
                    .bTE=.params$beta.TE[j], .bTLE=.params$beta.TLE[j])
      
      ##### Fit Analysis Model #####
      m = lm( Y ~ Te*L*E, data=ds )
      
      # fill in new value of rejection list
      rej[i] = ( anova(m)["Te:L:E", "Pr(>F)"] ) < 0.05  # p-value for 3-way interaction
    }
    
    # prob of rejection across reps
    p.reject = mean(rej)
    .params$Power[j] = p.reject
  }
  return(.params)
}


# make parameters dataframe
n.lab = seq(300,600,50)
#n.lab = c(200, 250, 300, 350, 400, 450)
n.turk = c( 800, 1200, 2000)
bTE = 0
bTLE = sd(d$Likelihood) * c(0.25, 0.5, 0.75)  # Cohen's d
params = expand.grid(N.Turk = n.turk, N.Lab = n.lab, beta.TE = bTE, beta.TLE = bTLE, Power = NA)

# run simulation
res = power(params, .reps=2000)


############### PLOT ###############

# cast as factor for plotting joy
res$N.Turk = as.factor(res$N.Turk)
res$N.Lab = as.factor(res$N.Lab)
res$beta.TLE = as.factor(res$beta.TLE)
res$ES = as.factor( round( as.numeric(as.character(res$beta.TLE)) / sd(d$Likelihood), 2 ) ) # "effect size" of coefficient on my own terms

ticks = seq(0.3, 1, 0.05)
colors = c("goldenrod2", "goldenrod3", "goldenrod4")

library(ggplot2)
ggplot( data=res, aes(x=N.Lab, y=Power, group=N.Turk, color=N.Turk ) ) +
  geom_point(size=0) + geom_line(lwd=1.2) + theme_bw() +
  ylab("Estimated Power") + facet_wrap(~ES) +
  scale_y_continuous(breaks=ticks, labels = ticks) + 
  geom_hline(yintercept = 0.8, linetype=2, color="red") +
  geom_hline(yintercept = 0.9, linetype=2, color="red") +
  ggtitle( expression( over( beta[plain(T*L*S)] , sigma[Y] ) ) ) +
  xlab("Lab N (total)") +
  scale_color_manual(values=colors)



########################### POWER TO DETECT ORIGINALLY REPORTED INTERACTION JUST IN SIMILAR SITES ###########################

# from original study
bT = 1.03
bTL = 1.54
bL = 0.94  # from RPP


##### Fn: Simulate 1 Dataset #####
sim_data = function(N, .error.sd=e) {
  
  # simulate covariates
  # ignore that there are multiple labs
  L = rbinom( n=N, size=1, prob=0.5 )  # random assignment to load
  Te = rbinom( n=N, size=1, prob=0.5 )  # and to tempt
  
  # simulate continuous outcome (perceived likelihood from 0-10) = linear predictor + Normal error
  means = c( b0 + bT*Te + bL*L + bTL*Te*L )
  length(means)
  Y = rnorm( N, mean = means, sd=.error.sd )
  return( data.frame(Y, L, Te) )
}



# initialize rejection vector
rej=c()
  
for (i in 1:1000) {
    if(i %% 1000 == 0) cat("/n/n", i)  # monitor
      
      ##### Simulate Dataset #####
      # using the appropriate row of parameters
      ds = sim_data( N=754 )
      
      ##### Fit Analysis Model #####
      m = lm( Y ~ Te*L, data=ds )
      
      # fill in new value of rejection list
      rej[i] = ( anova(m)["Te:L", "Pr(>F)"] ) < 0.05  # p-value for 2-way interaction
}
    
# prob of rejection across reps
( p.reject = mean(rej) )



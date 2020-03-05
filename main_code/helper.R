


######## Make Forest Plot ########

# NB: data must have column called "yval" (sequential integers to force correct ordering)
# also must have column called "is.pooled" (is the row for a pooled estimate?)

# in plot construction for LHS, I hard-coded the fontfaces (i.e. which strings are bold)
#  can easily be removed for unbolded plot

# est.name = variable name of point est
# lo.name = variable name of CI lower bound
# hi.name = variable name of CI upper bound
# file.name = name of file to write plot to
# site.name = variable name of studies
# n.name = variable name of sample sizes
# string.name = variable name of string to be printed in the "Estimate and CI column"
# pval.name = variable name of p-value 
# xlab = passed to ggplot
# min.x = passed to ggplot for x-axis
# max.x = passed to ggplot for x-axis


awesome_forest = function( data, est.name, lo.name, hi.name, file.name,
                           site.name = "site", 
                           n.name = "site.n",
                           string.name,
                           pval.name,
                           xlab,
                           min.x,
                           max.x
                           ) {

  # you can have any colors you want, as long as they're black and orange.
  colors = c("black", "orange")
  
  # RHS of forest plot
  RHS = 
    ggplot(data) + 
    theme_bw() + 
    aes( x = as.numeric( data[[est.name]] ),
        xmin = as.numeric( data[[lo.name]] ), xmax = as.numeric( data[[hi.name]] ),
        y = as.numeric(yval),
        color = is.pooled,  # use different color, plot character, and size for pooled ests
        size = is.pooled,
        shape = is.pooled ) + 
    scale_color_manual( values = colors ) +
    scale_size_manual( values = c(2,4) ) +
    geom_point( ) + 
    geom_errorbarh(height = 0.3, size=0.8 ) + 
    geom_vline(xintercept = 0) + 
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.border = element_blank(),
      legend.position = "none"
    )  +
    scale_x_continuous( limits=c(min.x, max.x), breaks=seq(min.x, max.x, .5) ) +
    scale_y_continuous( limits=c( 0, max( data$yval ) ), breaks=seq( 0, max( data$yval ), 1 ) ) +
    xlab(xlab)
  
  
  LHS =
    ggplot(data) + 
    theme_bw() + 
    aes(y = yval) + 
    
    # site names
    # fontface: bold some of the site names
    geom_text( aes( label = data[[site.name]], x = 0, fontface = ifelse( site %in% c("Original study", "RPP protocol", "Similar sites", "Dissimilar sites") | is.na(site), "bold", "plain" ), hjust = 0 ) ) + 
    
    # sample size
    # fontface: bold only the first header row, where site is NA
    geom_text( aes( label = data[[n.name]], x = 1, fontface = ifelse( is.na(site), "bold", "plain" ) ), hjust = 0 ) + 
    
    # point estimate and CI
    # fontface: bold only the first header row, where site is NA
    geom_text( aes( label = data[[string.name]], x = 1.75, fontface = ifelse( is.na(site), "bold", "plain" ) ), hjust = 0 ) + 
    
    # p-value
    # fontface: bold only the first header row, where site is NA
    geom_text(aes( label = data[[pval.name]], x = 3, fontface = ifelse( is.na(site), "bold", "plain" ) ), hjust = 0 ) +
    
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank() 
    ) + 
    xlim(0, 4) +
    ylim(0, max(data$yval))
  
  
  png(filename = file.name, width = 16, height = 6, units = "in", res = 300)
  
  pdf(file = file.name, width = 16, height = 6)
  
  grid.draw( gridExtra:::cbind_gtable( ggplotGrob(LHS), ggplotGrob(RHS), size = "last" ) )
  
  dev.off()
}




# Fn: for use in forest plot
#   inserts row into dataframe at specified position (row number)
#  and adds a "spacer" row of NAs before it

insert_at = function( row, df, position ) {
  if (position == 1) {
    as.data.frame( rbind(row, df) )
  }
  else {
    top = df[ 1:(position-1), ]
    bottom = df[ position:nrow(df), ]
    spacer.row = rep( NA, nrow(df) )
    as.data.frame( rbind( top, spacer.row, row, bottom ) )
  }
}


# Fn: compute load-stratified Cohen's d within a site (uncorrected to match original study)
# load: 1 or 0 for which subset to use
site_cohen = function( .d, .load ) {
  temp = .d[ .d$load == .load, ]
  
  diff = mean( temp$lkl[ temp$tempt == 1 ] ) - mean( temp$lkl[ temp$tempt == 0 ] )
  
  sd1 = sd( temp$lkl[ temp$tempt == 1 ] )
  n1 = length( temp$lkl[ temp$tempt == 1 ] )
  sd0 = sd( temp$lkl[ temp$tempt == 0 ] )
  n0 = length( temp$lkl[ temp$tempt == 0 ] )
  
  num = (n1 - 1) * sd1^2 + (n0 - 1) * sd0^2
  denom = n1 + n0 - 2
  sd.pool = sqrt( num / denom )
  
  return( diff / sd.pool )
}

# Fn: compute Hedges' g within site
# effect: compute for interaction or main effect?
site_hedges = function( .d,
                        effect = "interaction" ) {
  
  if ( effect == "interaction" ) {
    # difference on raw mean difference scale corresponding to the interaction (equal to a - b - c + d because
    #  saturated model)
    mod = lm(formula = lkl ~ tempt * load, data = .d)
    # residual (aka conditional) variance
    sd.pool = sigma(mod)
    
    # rescale the outcome by sd.pool, treated as known
    .d$lkl.std = .d$lkl/sd.pool
    # refit the model to get the standardized mean difference
    
    mod.std = lm(formula = lkl.std ~ tempt * load, data = .d)
    d = coef(mod.std)[["tempt:load"]]
    var.d = vcov(mod.std)[["tempt:load", "tempt:load"]]
    
    # sanity check vs. raw model: matches
    # coef(mod)[["tempt:load"]] / sd.pool; d
    # vcov(mod)[["tempt:load", "tempt:load"]] * (1/sd.pool)^2; var.d
    
    # Hedges' g (Borenstein pg. 27)
    df = nrow(.d) - 4   # 4 instead of 2 because estimating an interaction (3 effects + intercept)
    J = ( 1 - ( 3 / ( 4*df - 1 ) ) )  # this will be almost 1
    g = J * d
    se.g = sqrt( J^2 * var.d )
    
    t.crit = qt( 0.975, df = df)
    
    return( list( g = g, 
                  se.g = se.g,
                  lo = g - se.g * t.crit,
                  hi = g + se.g * t.crit ) )
  }
  
  if ( effect == "main" ) {
    # difference on raw mean difference scale corresponding to the interaction (equal to a - b - c + d because
    #  saturated model)
    
    mod = lm(formula = lkl ~ tempt, data = .d[ .d$load == 0, ] )
    # residual (aka conditional) variance
    sd.pool = sigma(mod)
    
    # rescale the outcome by sd.pool, treated as known
    .d$lkl.std = .d$lkl/sd.pool
    # refit the model to get the standardized mean difference
    
    mod.std = lm(formula = lkl.std ~ tempt, data = .d[ .d$load == 0, ] )
    d = coef(mod.std)[["tempt"]]
    var.d = vcov(mod.std)[["tempt", "tempt"]]
    
    # sanity check vs. raw model: matches
    # coef(mod)[["tempt:load"]] / sd.pool; d
    # vcov(mod)[["tempt:load", "tempt:load"]] * (1/sd.pool)^2; var.d
    
    # Hedges' g (Borenstein pg. 27)
    df = nrow(.d) - 2 
    J = ( 1 - ( 3 / ( 4*df - 1 ) ) )  # this will be almost 1
    g = J * d
    se.g = sqrt( J^2 * var.d )
    
    t.crit = qt( 0.975, df = df)
    
    return( list( g = g, 
                  se.g = se.g,
                  lo = g - se.g * t.crit,
                  hi = g + se.g * t.crit ) )
  }
  

}

# Fn: rounding function that keeps trailing zeroes
my_round = function(x, digits) {
  format( round( x, digits ), nsmall = digits )
}

# Fn: calculate SE for sum of coefficients
# b1, b2: names of the two coefficients to add
# .mod: the lmer model object
lin_combo = function( b1, b2, .mod, difference = FALSE ) {

  # extract var-cov matrix of parameter estimates
  V = vcov(.mod)

  # default: sum
  if( difference == FALSE ) {
    SE = sqrt( V[b1, b1] + V[b2, b2] + 2 * V[b1, b2] )
    est = fixef(.mod)[b1] + fixef(.mod)[b2]
  }
  
  # otherwise: difference
  if( difference == TRUE ) {
    SE = sqrt( V[b1, b1] + V[b2, b2] - 2 * V[b1, b2] )
    est = fixef(.mod)[b1] - fixef(.mod)[b2]
  }
  
  lo = as.numeric( est - qnorm(0.975) * SE )
  hi = as.numeric( est + qnorm(0.975) * SE )
  
  # p-value from z-distribution
  pval = ( 1 - pnorm( abs(est / SE) ) ) * 2
  
  return( data.frame( est, lo, hi, pval, SE ) )
}


# Fn: calculate Wald p-value for LMM
# these will agree exactly with confint( , method="Wald" )
# unlike lmerTest's too-conservative Satterthwaite approximation
# .m: merMod object from lmer
z_pvals = function( .m ) {
  coefs = data.frame( coef( summary( .m ) ) )
  p = data.frame( 2 * ( 1 - pnorm( abs( coefs$t.value ) ) ) )
  row.names(p) = row.names(coefs)
  return(p)
}

# Fn: insert spacey elements in vectors for purely cosmetic forest plot reasons
# spaces are between site types
# "use.NA" = should we put NA instead of ""?
pretty_spaces = function(x, use.NA = FALSE){
  x2 = append( x, ifelse( use.NA, NA, "" ), after = 1)
  x2 = append( x2, ifelse( use.NA, NA, "" ), after = 3)
  x2 = append( x2, ifelse( use.NA, NA, "" ), after = 8)
}



################################ FNs FOR DIAGNOSTIC PLOTS ################################ 

# see section 3.11 of Veroniki (from Raudenbush 2009)
t2_lkl = function( yi, vi, t2, yi.orig, vi.orig ) {
  k = length(yi)
  wi = 1 / ( vi + t2 )
  
  # calculate the RE Mhat based on this particular t2
  Mhat = sum(yi * wi) / sum(wi)
  
  term1 = (k/2) * log(2*pi)
  term2 = 0.5 * sum( log(vi + t2) )
  term3 = 0.5 * sum( ( yi - Mhat )^2 / ( vi + t2 ) )
  term4 = 0.5 * log( sum( 1 / ( vi + t2 ) ) )
  
  return( -term1 - term2 - term3 - term4 )
}


diag_plots = function(yi, vi, yi.orig, vi.orig) {
  
  # fit meta-analysis
  .m = rma.uni( yi = yi,
                vi = vi,
                knha = TRUE ) 
  
  ##### Make Plotting Dataframe with Different Tau^2 #####
  # here using 1 as upper limit since MLE is actually 0
  t2.vec = seq(0, 1, .001)
  t2l = as.list(t2.vec)
  
  temp = lapply( t2l,
                 FUN = function(t2) {
                   suppressMessages( p_orig( orig.y = yi.orig,
                                             orig.vy = vi.orig,
                                             yr = .m$b,
                                             t2 = t2,
                                             vyr = .m$vb ) )
                 })
  
  # plotting df
  dp = data.frame( tau = sqrt(t2.vec),
                   V = t2.vec,
                   Porig = unlist(temp) )
  
  
  # fn: tau^2 vs. the estimated one
  ( breaks.x1 = seq( 0, max(dp$V), .1 ) )
  #g = function(x) x / .m$tau2
  #( breaks.x2 = seq( 0, max( g(dp$V) ), 1 ) )
  
  ##### **Plot 1: Tau^2 vs. Porig #####
  p1 = ggplot( data = dp,
               aes(x = V, 
                   y = Porig) ) +
    
    geom_vline(xintercept = .m$tau2,
               lty = 2,
               color = "red") +  # observed one
    
    geom_line(lwd = 1.2) +
    theme_classic() +
    xlab( bquote( tau["*"]^2 ) ) +
    ylab( bquote(P[orig]) ) +
    scale_x_continuous( limits = c(0, max(breaks.x1)),
                        breaks = breaks.x1
                        # sec.axis = sec_axis( ~ g(.),  # confounding strength axis
                        #                      name = bquote( tau["*"]^2 / hat(tau)^2 ),
                        #                      breaks=breaks.x2 )
                        ) +
    scale_y_continuous( breaks = seq(0,1,.05) )
  # 6 x 4
  
  ##### **Plot 2: Tau^2 vs. Mhat/Tau^2 ##### 
  p2 = ggplot( data = dp,
               aes(x = V, 
                   y = .m$b / V ) ) +
    
    geom_vline(xintercept = .m$tau2,
               lty = 2,
               color = "red") +  # observed one
    
    geom_line(lwd = 1.2) +
    theme_classic() +
    xlab( bquote( tau["*"]^2 ) ) +
    ylab( bquote( hat(mu) / tau["*"]^2 ) ) +
    scale_x_continuous( limits = c(0, max(breaks.x1)),
                        breaks = breaks.x1
                        # sec.axis = sec_axis( ~ g(.),  # confounding strength axis
                        #                      name = bquote( tau["*"]^2 / hat(tau)^2 ),
                        #                      breaks=breaks.x2 )
                        ) +
    scale_y_continuous( breaks = seq(0,80,10) )
  # 6 x 4
  
  ##### **Plot 3: Marginal Log-Lkl of Tau^2 ##### 
  temp = lapply( t2l,
                 FUN = function(t2) {
                   # log-lkl itself
                   t2_lkl( yi = yi,
                           vi = vi,
                           t2 = t2 )
                   
                   # lkl ratio vs. the MLE
                   exp( t2_lkl( yi = yi,
                                vi = vi,
                                t2 = t2 ) ) / exp( t2_lkl( yi = yi,
                                                           vi = vi,
                                                           t2 = .m$tau2 ) )
                 })
  
  # plotting df
  dp = data.frame( tau = sqrt(t2.vec),
                   V = t2.vec,
                   lkl = unlist(temp) )
  
  p3 = ggplot( data = dp,
               aes(x = V, 
                   y = lkl ) ) +
    
    geom_vline(xintercept = .m$tau2,
               lty = 2,
               color = "red") +  # observed one
    
    geom_line(lwd = 1.2) +
    theme_classic() +
    xlab( bquote( tau["*"]^2 ) ) +
    ylab( "Marginal likelihood ratio of " ~ tau["*"]^2 ~ " vs. " ~ hat(tau)^2 ) +
    scale_x_continuous( limits = c(0, max(breaks.x1)),
                        breaks = breaks.x1
                        # sec.axis = sec_axis( ~ g(.),  # confounding strength axis
                        #                      name = bquote( tau["*"]^2 / hat(tau)^2 ),
                        #                      breaks=breaks.x2 )
                        ) +
    scale_y_continuous( limits = c(0,1),
                        breaks = seq(0,1,.1) )
  
  return( list(p1, p2, p3) )
  
}



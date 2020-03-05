
##################################### FUNCTIONS TO MAKE AWESOME FOREST PLOT #####################################

# Fn: for use in building plot-friendly dataframe
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

# Usage notes:
#
# 1.) data must have column called "yval" (sequential integers for each row to align LHS and RHS with each other)
#  also must have column called "is.pooled" (is the row for a pooled estimate?)
#
# 2.) in plot construction for LHS, I hard-coded the fontfaces (i.e. which strings are bold),
#  but those arguments can easily be removed for plain-font plot
#
# 3.) ggplot will throw warnings about removing rows with missing values
#  those are okay (just because of spacer rows for LHS of plot)
# 
# 4.) data must have column called "is.pooled" (TRUE/FALSE) indicating whether the estimate
#  is a site or the pooled one

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


awesome_forest = function( data,
                           est.name,
                           lo.name,
                           hi.name,
                           file.name,
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

  # LHS of forest plot
  LHS =
    ggplot(data) + 
    theme_bw() + 
    aes(y = yval) + 
    
    # site names
    # fontface: bold some of the site names
    geom_text( aes( label = data[[site.name]], x = 0,
                    fontface = ifelse( data[[site.name]] %in% c("Original study", "RPP protocol", "Similar sites", "Dissimilar sites")
                                       | is.na(data[[site.name]]), "bold", "plain" ), hjust = 0 ) ) + 
    
    # sample size
    # fontface: bold only the first header row, where site is NA
    geom_text( aes( label = data[[n.name]], x = 1,
                    fontface = ifelse( is.na(data[[site.name]]), "bold", "plain" ) ), hjust = 0 ) + 
    
    # point estimate and CI
    # fontface: bold only the first header row, where site is NA
    geom_text( aes( label = data[[string.name]], x = 1.75, fontface = ifelse( is.na(data[[site.name]]), "bold", "plain" ) ), hjust = 0 ) + 
    
    # p-value
    # fontface: bold only the first header row, where site is NA
    geom_text(aes( label = data[[pval.name]], x = 3, fontface = ifelse( is.na(data[[site.name]]), "bold", "plain" ) ), hjust = 0 ) +
    
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
  
  grid.arrange(LHS, RHS, ncol=2)
  #grid.draw( gridExtra:::cbind.gtable( ggplotGrob(LHS), ggplotGrob(RHS), size = "last" ) )
  dev.off()
}



# ##################################### EXAMPLE #####################################
# 
# # read in meta-analysis dataset
# 
# s = read.csv("sites.csv")
# 
# 
# ##### Format Sites Dataframe For LHS of Forest Plot ##### 
# 
# # NB: all that matters is the end result of the formatting
# #  to see the end result, read in "temp.csv"
# #  you could, e.g., produce the temp.csv file manually by inserting rows/columns in Excel
# 
# temp = sites[ , names(sites) %in% c("site", "site.n", "site.main.est", "site.main.lo", "site.main.hi", "site.main.pval",
#                                     "site.int.est", "site.int.lo", "site.int.hi", "site.int.pval", "is.mturk", "group", "site.n") ]
# 
# # add row for original study
# temp = rbind( data.frame( site = "Original study", site.n = 120, site.main.est = yi.orig.main, site.main.lo = orig.main.lo, site.main.hi =  orig.main.hi, site.main.pval = pval.orig.main,
#                           site.int.est = yi.orig.int, site.int.lo = orig.int.lo, site.int.hi = orig.int.hi, site.int.pval = pval.orig.int,
#                           is.mturk = FALSE, group = "d.original"), temp )
# 
# temp$site = as.character(temp$site)
# 
# # insert header rows to be printed in bold
# temp = insert_at( row = c( "RPP protocol", rep( NA, ncol(temp)-1 ) ),
#                   df = temp,
#                   position = 2
# )
# 
# new.row = c( "Similar sites", rep( NA, ncol(temp)-1 ) )
# new.row[ names(temp) == "site.main.est" ] = main.sim$est
# new.row[ names(temp) == "site.main.lo" ] = main.sim$lo
# new.row[ names(temp) == "site.main.hi" ] = main.sim$hi
# new.row[ names(temp) == "site.main.pval" ] = main.sim$pval
# new.row[ names(temp) == "site.int.est" ] = int.sim$est
# new.row[ names(temp) == "site.int.lo" ] = int.sim$lo
# new.row[ names(temp) == "site.int.hi" ] = int.sim$hi
# new.row[ names(temp) == "site.int.pval" ] = int.sim$pval
# new.row[ names(temp) == "site.n" ] = sum( sites$site.n[ sites$group == "b.similar" ] )
# temp = insert_at( row = new.row, df = temp, position = 5 )
# 
# 
# new.row = c( "Dissimilar sites", rep( NA, ncol(temp)-1 ) )
# new.row[ names(temp) == "site.main.est" ] = main.dis.m3$est
# new.row[ names(temp) == "site.main.lo" ] = main.dis.m3$lo
# new.row[ names(temp) == "site.main.hi" ] = main.dis.m3$hi
# new.row[ names(temp) == "site.main.pval" ] = main.dis.m3$pval
# new.row[ names(temp) == "site.int.est" ] = int.dis.m3$est
# new.row[ names(temp) == "site.int.lo" ] = int.dis.m3$lo
# new.row[ names(temp) == "site.int.hi" ] = int.dis.m3$hi
# new.row[ names(temp) == "site.int.pval" ] = int.dis.m3$pval
# new.row[ names(temp) == "site.n" ] = sum( sites$site.n[ sites$group == "c.dissimilar" ] )
# temp = insert_at( row = new.row, df = temp, position = 11 )
# 
# # cast stuff as numeric
# temp$site.main.est = as.numeric(temp$site.main.est)
# temp$site.main.lo = as.numeric(temp$site.main.lo)
# temp$site.main.hi = as.numeric(temp$site.main.hi)
# temp$site.main.pval = as.numeric(temp$site.main.pval)
# 
# temp$site.int.est = as.numeric(temp$site.int.est)
# temp$site.int.lo = as.numeric(temp$site.int.lo)
# temp$site.int.hi = as.numeric(temp$site.int.hi)
# temp$site.int.pval = as.numeric(temp$site.int.pval)
# 
# # build plot-friendly string with point estimate and CI
# temp$string.main = paste( my_round( temp$site.main.est, 2 ), " [", my_round( temp$site.main.lo, 2 ), ", ", my_round( temp$site.main.hi, 2 ), "]", sep="" )
# temp$string.int = paste( my_round( temp$site.int.est, 2 ), " [", my_round( temp$site.int.lo, 2 ), ", ", my_round( temp$site.int.hi, 2 ), "]", sep="" )
# temp$site.main.pval = my_round( temp$site.main.pval, 2 )
# temp$site.int.pval = my_round( temp$site.int.pval, 2 )
# 
# # this avoids printing "NA" as a string on the plot
# temp$string.main[ is.na(temp$site) | temp$site == "RPP protocol" ] = NA
# temp$site.main.pval[ is.na(temp$site) | temp$site == "RPP protocol" ] = NA
# temp$string.int[ is.na(temp$site) | temp$site == "RPP protocol" ] = NA
# temp$site.int.pval[ is.na(temp$site) | temp$site == "RPP protocol" ] = NA
# 
# 
# # add row to appear at the very top with column headers
# label.row = rep(NA, ncol(temp))
# label.row[ names(temp) == "site.n"] = "Analyzed n"
# label.row[ names(temp) == "string.main"] = "Estimate and 95% CI"
# label.row[ names(temp) == "string.int"] = "Estimate and 95% CI"
# label.row[ names(temp) == "site.int.pval"] = "p-value"
# temp = insert_at( row = label.row,
#                   df = temp,
#                   position = 1 )
# 
# # set a y-axis value for each label
# # ensures alignment between LHS and RHS of forest plot
# temp$yval = rev( seq( 1, nrow(temp), by = 1) )
# 
# # add spaces for indentation
# temp$site[ ! temp$site %in% c("Original study", "RPP protocol", "Similar sites", "Dissimilar sites") & ! is.na(temp$site) ] = paste("   ", temp$site[ ! temp$site %in% c("Original study", "RPP protocol", "Similar sites", "Dissimilar sites") & ! is.na(temp$site) ], sep="")
# 
# # for use in plot legend (colors, plot shapes)
# temp$is.pooled = ifelse( temp$site %in% c("Similar sites", "Dissimilar sites"), TRUE, FALSE )
# 
# # save plot-friendly dataframe to illustrate its structure
# #write.csv(temp, "temp.csv")
# 
# 

##### Make Forest Plot ##### 

# awesome_forest( data=temp, est.name="site.main.est",
#                 lo.name="site.main.lo", hi.name = "site.main.hi",
#                 file.name = "main_forest",
#                 site.name = "site", 
#                 n.name = "site.n",
#                 string.name = "string.main",
#                 pval.name = "site.main.pval",
#                 xlab = "Main effect estimate",
#                 min.x = -2.5, max.x = 2.5 )


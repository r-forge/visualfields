progols <- function( age, index, projyears = 5, xlab = "age", ylab = "md", txtfont = "mono", pointsize = 12, cex = 1 ) {

  xreg <- c( min( age ), max( age ) + projyears )
  xlim    <- NULL
  ylim    <- NULL
  xlim[1] <- xreg[1]
  xlim[2] <- xreg[2] + 1
  ylim <- c( min( index ) - 1, max( index ) + 1 )
# get regression
  mdreg <- lm( index ~ age )
  pval  <- summary( mdreg )$coefficients[2,4]
  yreg  <- mdreg$coefficients[1] + mdreg$coefficients[2] * xreg

  ops     <- par()$ps
  ofamily <- par()$family
  oplt    <- par()$plt
  par( ps     = pointsize )
  par( family = txtfont )
  par( plt    = c( 0.25, 1.0, 0.35, 0.9 ) )
  
  plot( age, index, axes = FALSE, ann = FALSE, xlim = xlim, ylim = ylim )
  lines( xreg, yreg )
  axis( 1, las = 1, tcl = -.3, lwd = 0.5, lwd.ticks = 0.5 )
  axis( 2, las = 1, tcl = -.3, lwd = 0.5, lwd.ticks = 0.5 )
  grid( nx = NA, ny = 4, lty = "solid", "gray" )
  box()
  title( xlab = xlab, mgp = c( 2, 1, 0 ) )
  title( ylab = ylab, mgp = c( 2.3, 1, 0 ) )

  par( new    = FALSE )
  par( ps     = ops )
  par( family = ofamily )
  par( plt    = oplt )
}

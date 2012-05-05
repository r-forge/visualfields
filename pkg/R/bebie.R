bebie <- function( td, diff = TRUE, percentiles = TRUE, correction = TRUE, txtfont = "mono", pointsize = 7, cex = 1 ) {

  if( nrow( td ) > 1 ) {
    stop( "Error! only one visual field to use here" )
  }

  xlab <- "rank"
  linewdt <- 1.5
  if( diff ) {
    ylab <- "dB difference"
  } else {
    ylab <- "dB"
  }

  tdr <- tdrank( td )
  tdr <- as.numeric( tdr[,vfsettings$locini:ncol( tdr )] )

# set limits
  xlim <- c( 1, length( tdr ) )
  if( diff ) ylim <- c( -15, 5 ) else ylim <- c( -20, 10 )

# get reference
  evaltxt <- paste( "nv$", td$tpattern, "_", td$talgorithm, "$nvtdrank", sep = "" )
  tdrref <- as.numeric( eval( parse( text = evaltxt ) )$mtdr )

# get differences
  if( diff ) tdr <- tdr - tdrref
# get correction
  if( correction ) {
    evaltxt <- paste( "vfsettings$", td$tpattern, "$locrPD", sep = "" )
    loc <- eval( parse( text = evaltxt ) )
    tdrc <- tdr - tdr[loc]
  }
# get percentiles
  if( percentiles ) {
    if( diff ) {
      evaltxt <- paste( "nv$", td$tpattern, "_", td$talgorithm, "$perctdrankadj", sep = "" )
    } else {
      evaltxt <- paste( "nv$", td$tpattern, "_", td$talgorithm, "$perctdrank", sep = "" )
    }
    tdrperc <- eval( parse( text = evaltxt ) )
  }
  ops     <- par()$ps
  ofamily <- par()$family
  par( ps     = pointsize )
  par( family = txtfont )

  if( diff ) {
    plot(c( xlim[1], xlim[2] ), c( 0, 0 ), axes = FALSE, ann = FALSE, xlim = xlim, ylim = ylim, type = "n" )
  } else {
    plot( tdrref, axes = FALSE, ann = FALSE, xlim = xlim, ylim = ylim, type = "l", lwd = linewdt )
  }

  axis( 1, las = 1, tcl = -.3, lwd = 0.5, lwd.ticks = 0.5 )
  axis( 2, las = 1, tcl = -.3, lwd = 0.5, lwd.ticks = 0.5 )
  grid( nx = NA, ny = NULL, lty = "solid", "gray" )
  box()
  title( xlab = xlab, mgp = c( 2, 1, 0 ) )
  title( ylab = ylab, mgp = c( 2.3, 1, 0 ) )

  if( percentiles ){
    for( i in 1:( ncol( tdrperc ) - 1 ) ) {
      lines( tdrperc[,i], col = rgb( red = nv$pmapsettings$red[i], green = nv$pmapsettings$green[i], blue = nv$pmapsettings$blue[i] ), lwd = linewdt )
    }
    lines( tdrperc[,ncol( tdrperc )], col = rgb( red = nv$pmapsettings$red[nrow( nv$pmapsettings )], green = nv$pmapsettings$green[nrow( nv$pmapsettings )], blue = nv$pmapsettings$blue[nrow( nv$pmapsettings )] ), lwd = linewdt )
  }

  points( tdr, xlim = xlim, ylim = ylim, pch = 1, cex = cex )
  if( correction ) points( tdrc, xlim = xlim, ylim = ylim, pch = 16, cex = cex )

  par( new    = FALSE )
  par( ps     = ops )
  par( family = ofamily )

}

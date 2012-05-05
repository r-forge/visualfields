vfplotloc <- function( vals, eye, patternMap, outerColor, innerColor = NULL, bs = NULL, axesCol = "black",
                       txtfont = "mono", pointsize = 7, txtcolor = "black", xminmax = 29, yminmax = 29,
                       outerSymbol = "circles", innerSymbol = "circles",
                       outerSize = 1, innerSize = 1, outerInch = 0.2, innerInch = 0.1,
                       lengthLines = 2.5, thicknessLines = 2 ) {

# init
  if( is.null( innerColor ) ) innerColor <- matrix( rep( c( 1, 1, 1 ), length( vals ) ), length( vals ), 3 )
  oplt    <- par()$plt
  ops     <- par()$ps
  ofamily <- par()$family
  par( plt = c( 0, 1, 0, 1 ) )
  par( ps = pointsize )
  par( family = txtfont )
# get lines denoting the orientation of the RNF bundle
  coords <- vfsegmentCoord( patternMap, length = lengthLines )
  
  # left/right eye
  if( eye == "OS" ) {
    patternMap$xod <- -patternMap$xod
    if( !is.null( bs ) ) bs[1] <- -bs[1]
    coords$x1      <- -coords$x1
    coords$x2      <- -coords$x2
  }
  plot( patternMap$xod, patternMap$yod, type = "n", axes = FALSE, xlab = "", ylab = "", asp = 1, xlim = c( -xminmax, xminmax ), ylim = c( -yminmax, yminmax ) )
# add an x in the blindspot location
  if( !is.null( bs ) ) points( bs[1], bs[2], pch = 4, lwd = thicknessLines, cex = 5 )
# plot lines denoting the orientation of the RNF bundle
  segments( coords$x1, coords$y1, coords$x2, coords$y2, lwd = thicknessLines )
# control axes
  axis( 1, pos = 0, labels = FALSE, lwd.ticks = 0, at = c( -xminmax, xminmax ), col = axesCol )
  axis( 2, pos = 0, labels = FALSE, lwd.ticks = 0, at = c( -yminmax, yminmax ), col = axesCol )  
# Rectangles require matrix with 2 columns: 1st column specifies the width, 2nd column specifies the height
  outerDimensions <- t( matrix( data = rep( outerSize, nrow( patternMap ) ),nrow = length( outerSize ), ncol = nrow( patternMap ) ) )
  evaltxt <- paste( "symbols( patternMap$xod, patternMap$yod, " , outerSymbol, " = outerDimensions, add = TRUE, inches = outerInch, bg = rgb( outerColor[,1] , outerColor[,2], outerColor[,3] ), fg = rgb( outerColor[,1] , outerColor[,2], outerColor[,3] ), lwd = 1 )", sep = "" )
  eval( parse( text = evaltxt ) )
  innerDimensions <- t( matrix( data = rep( innerSize, nrow( patternMap ) ),nrow = length( innerSize ), ncol = nrow( patternMap ) ) )
  evaltxt <- paste( "symbols( patternMap$xod, patternMap$yod, ", innerSymbol," = innerDimensions, add = TRUE, inches = innerInch, bg = rgb( innerColor[,1] , innerColor[,2], innerColor[,3] ), fg = rgb( innerColor[,1] , innerColor[,2], innerColor[,3] ), lwd = 1 )", sep = "" )
  eval( parse( text = evaltxt ) )
  
  text( patternMap$xod, patternMap$yod, col = txtcolor, labels = vals, adj = 0.525 )
  
  par( plt    = oplt )
  par( ps     = ops )
  par( family = ofamily )

}
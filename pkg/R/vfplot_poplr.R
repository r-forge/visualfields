vfplot_poplr <- function( sl, pval, vfinfo, newWindow = FALSE, txtfont = "mono", pointsize = 7,
                          width = 6, xminmax = 29, yminmax = 29,
                          outerSymbol = "circles", innerSymbol = "circles",
                          outerSize = 1, innerSize = 1,
                          outerInch = 0.24, innerInch = 0.12,
                          lengthLines = 3.0, thicknessLines = 2 ) {

# check that symbol input arguments are consistent
# sizes must be arrays, not matrices
  if( is.matrix(outerSize ) ) {
    stop( "Error! outerSize cannot be a matrix" )
  }
  if( is.matrix( innerSize ) ) {
    stop( "Error! innerSize cannot be a matrix" )
  }
# circles and squares are specified by one number
  if( outerSymbol == c( "circles" ) || outerSymbol == c( "squares" ) ) {
    if( length( outerSize ) != 1 ) {
      stop( "Error! length of outerSize should be 1 for circles or squares" )
    }
  }
  if( innerSymbol == c( "circles" ) || innerSymbol == c( "squares" ) ) {
    if( length( innerSize ) != 1 ) {
      stop( "Error! length of outerSize should be 1 for circles or squares" )
    }
  }
# rectangles are specified by two numbers
  if( outerSymbol == c( "rectangles" ) ) {
    if( length( outerSize ) != 2 ) {
      stop( "Error! length of outerSize should be 2 for rectangles" )
    }
  }
# rectangles are specified by two numbers
  if( innerSymbol == c( "rectangles" ) ) {
    if( length( innerSize ) != 2 ) {
      stop( "Error! length of outerSize should be 2 for rectangles" )
    }
  }
# stars are specifed by more than two numbers
  if( outerSymbol == c( "stars" ) ) {
    if( length( outerSize ) < 3 ) {
      stop( "Error! outerSize should be greater than or equal to 3 for stars" )
    }
  }
  if( innerSymbol == c( "stars" ) ) {
    if( length( innerSize ) < 3 ) {
      stop( "Error! outerSize should be greater than or equal to 3 for stars" )
    }
  }

# construct patternmap
  evaltxt <- paste( vfinfo$tperimetry, "locmap$", vfinfo$tpattern, sep = "" )
  patternMap <- eval( parse( text=evaltxt ) )

# remove blindspot
  evaltxt <- paste( "vfsettings$", vfinfo$tpattern, "$bs", sep = "" )
  bspos <- eval( parse( text = evaltxt ) )
  patternMap <- patternMap[-bspos,]

# create a new window and plot data in it
# window rescale is set to fixed to ensure re-sizing window doesn't re-size the plot
  height <- width * yminmax / xminmax
  if( newWindow ) {
    if( .Platform$OS.type == "unix" ) {
      if( Sys.info()["sysname"] == "Darwin" ) {
        quartz( width = width, height = height, dpi = 85 )
      } else {
        x11( xpos = 0, ypos = 0, width = width, height = height )
      }
    } else{
      windows( xpos = 0, ypos = 0, width = width, height = height, rescale = "fixed" )
    }
  }
  pval <- 100 * pval
  pvalc <- rep( c( 100 ), length( pval ) )
  pvalc[which( pval <= nv$pmapsettings$cutoffs[1] )] <- nv$pmapsettings$cutoffs[1]
  for( i in 2:( length( nv$pmapsettings$cutoffs ) - 1) ) pvalc[which( pval > nv$pmapsettings$cutoffs[i-1] & pval <= nv$pmapsettings$cutoffs[i] )] <- nv$pmapsettings$cutoffs[i]
  plotColor  <- vfcolormap( as.data.frame( pvalc ) )

  sl <- round( 10 * sl ) / 10
  vfplotloc( sl, eye = vfinfo$seye, patternMap = patternMap, outerColor = plotColor,
             txtfont = txtfont, pointsize = pointsize,
             xminmax = xminmax, yminmax = yminmax,
             outerSymbol = outerSymbol, innerSymbol = innerSymbol,
             outerSize = outerSize, innerSize = innerSize,
             outerInch = outerInch, innerInch = innerInch,
             lengthLines = lengthLines, thicknessLines = thicknessLines )

}
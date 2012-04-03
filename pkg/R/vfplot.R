vfplot <- function( vf, plotType, notSeenAsBlack = TRUE, newWindow = FALSE,
                    txtfont = "mono", pointsize = 7,
                    width = 5, xminmax = 29, yminmax = 29,
                    outerSymbol = "circles", innerSymbol = "circles",
                    outerSize = 1, innerSize = 1,
                    outerInch = 0.2, innerInch = 0.1,
                    lengthLines = 2.5, thicknessLines = 2 ) {

# check that vf has only 1 entry
  if( nrow( vf ) > 1 ) {
    stop("Error! vf cannot have more than 1 rows")
  }

# Check that symbol input arguments are consistent
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

# construct the pattern string based on the pattern type
  evaltxt <- paste( "vfsettings$", vf$tpattern, "$locnum", sep = "" )
  loc_num <- eval( parse( text = evaltxt ) )

# construct patternmap
  evaltxt <- paste( vf$tperimetry, "locmap$", vf$tpattern, sep = "" )
  patternMap <- eval( parse( text=evaltxt ) )
# left/right eye
  if( vf$seye == "OS" ) {
    patternMap$xod <- -patternMap$xod
  }

  if( plotType != "vf" ) {
# get bs
    evaltxt <- paste( "vfsettings$", vf$tpattern, "$bs", sep = "" )
    bspos <- eval( parse( text = evaltxt ) )
  }

# read in the plotType and decide what to do
  if( plotType == "vf" ) {
    dev  <- vf
  }  

# if plot type id 'td' then calculate total deviation and total deviation probability
  if( plotType == "td" ) {
    dev  <- tdval( vf )
    devP <- tdpmap( dev )
  }  
    
# if plot type id 'pd' then first calculate total deviation
# use the toal deviation to calculate probabilty deviation 
  if( plotType == "pd" ) {
    dev  <- tdval( vf )
    dev  <- pdval( dev )
    devP <- pdpmap( dev )
  }
    
# getRGB will return a table with the red, green and blue intensity values 
# corresponding to pattern deviation at each location
  if( plotType == "vf" ) {
    plotColor  <- vfgrayscale( dev[,vfsettings$locini:( vfsettings$locini + loc_num - 1 )], age = vf$sage, pattern = vf$tpattern, algorithm = vf$talgorithm )
    cloneDev   <- as.character( round( dev[,vfsettings$locini:( vfsettings$locini + loc_num - 1 )] ) )
  }  else {
    plotColor  <- vfColorMap( devP[,vfsettings$locini:( vfsettings$locini + loc_num - 1 )] )
    patternMap <- patternMap[-bspos,]
    plotColor  <- plotColor[-bspos,]
# exclude blind spot locations
    dev <- dev[,-( vfsettings$locini + bspos - 1 )]
    if( notSeenAsBlack ) plotColor[which( vf[vfsettings$locini:( vfsettings$locini + loc_num - length( bspos ) - 1 )] == 0),] <- 0 # IMF NOT ROUND!!!
    cloneDev <- as.character( round( dev[,vfsettings$locini:( vfsettings$locini + loc_num - length( bspos ) - 1 )] ) )
  }

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
  vfplotloc( cloneDev, patternMap = patternMap, outerColor = plotColor, bs = c( vf$sbsx, vf$sbsy ),
             txtfont = txtfont, pointsize = pointsize,
             xminmax = xminmax, yminmax = yminmax,
             outerSymbol = outerSymbol, innerSymbol = innerSymbol,
             outerSize = outerSize, innerSize = innerSize,
             outerInch = outerInch, innerInch = innerInch,
             lengthLines = lengthLines, thicknessLines = thicknessLines )

}
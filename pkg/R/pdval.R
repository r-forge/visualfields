pdval <- function( td ) {
  pd <- td

  for( i in 1:nrow( pd ) ) {
# get how many locations we need to look at
    texteval <- paste( "vfsettings$", pd$tpattern[i], "$locnum", sep = "" )
    locnum <- eval( parse( text = texteval ) )
# get PD values from obtained gh
    pd[i,vfsettings$locini:vfsettings$locini:( vfsettings$locini - 1 + locnum )] <-
      pd[i,vfsettings$locini:( vfsettings$locini - 1 + locnum )] - ghpostd( td[i,] )
  }

  return( pd )
}

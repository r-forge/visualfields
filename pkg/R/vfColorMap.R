vfColorMap <- function( map ) {

  rgbval                    <- NULL
  rgbval$red[1:ncol(map)]   <- c( NA )
  rgbval$green[1:ncol(map)] <- c( NA )
  rgbval$blue[1:ncol(map)]  <- c( NA )
  rgbval                    <- as.data.frame( rgbval )

  for( i in 1:length( nv$pmapsettings$cutoffs ) ) {
    idx <- which( map == nv$pmapsettings$cutoffs[i] ) 
    if( length( idx ) > 0 ) rgbval[idx,] <- nv$pmapsettings[i,2:ncol( nv$pmapsettings )]
  }  

  return ( rgbval )
}

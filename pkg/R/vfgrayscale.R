vfgrayscale <- function( vf, age, pattern, algorithm ) {

# get the norm data and calculate normal age-corrected sensitivities
  texteval <- paste( "nv$", pattern, "_", algorithm, "$agelm", sep = "" )
  agelm    <- eval( parse( text = texteval ) )
  vals     <- agelm$intercept + agelm$slope * age
  idx      <- which( is.na( vals ) )
  if( length( idx ) > 0 ) vals <- vals[-idx]
  vf <- as.numeric( vf )
  vf <- vf / max( vals )
  vf[which( vf > 1 )] <- 1
  return ( matrix( rep( vf, 3 ), nrow = length( vf ), ncol = 3 ) )
}

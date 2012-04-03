poplr <- function( vf, nperm = 5000, type = "slr", sl_test = NULL, typecomb = "fisher", details = FALSE ) {
##############
# input checks
##############
# check that all rows in vf belong to the same subject, the same test, the same perimetry
# testing and the same algorithm
  if( length( unique( vf$tperimetry ) ) > 1 |
      length( unique( vf$tpattern   ) ) > 1 |
      length( unique( vf$talgorithm ) ) > 1 |
      length( unique( vf$id ) ) > 1         |
      length( unique( vf$seye ) ) > 1 ) {
    stop( "all visual fields should belong to the same subject tested with the same perimeter and algorithm on the same locations" )
  }
  if( nperm < 5 ) stop( "Come on! At least 5 laps. Number of permutations was lower than 5" )
  if( nperm > 1000000 ) stop( "please don't! Don't use more than a million permutations!" )
  if( ( type != "slr" & !is.null( sl_test ) ) ) stop( "tests about slopes being larger than a value are only valid for slr analysis" )

# permutation matrix
  porder <- make.permSpace( c( 1:nrow( vf ) ), nperm )$permID

  res          <- NULL
  res$vfdata   <- vf[1,1:vfsettings$locini-1]
  res$nvisits  <- nrow( vf )
  res$nperm    <- nperm
  res$type     <- type
  res$typecomb <- typecomb
# get the p-value statitics of the permuation analysis ...
  pstat    <- poplr_pstat( vf, porder = porder, type = "slr",  sl_test = sl_test )
# ... and the actual analysis
  cstat    <- poplr_cstat( pstat$pval, typecomb = typecomb )
  res$pcomb_obs <- cstat$pcomb_obs
  if( type == "slr" ) {
    res$sl   <- pstat$sl[1,]
    res$int  <- pstat$int[1,]
    res$se   <- pstat$se[1,]
  } else if( type == "rank" ) {
    res$rho  <- pstat$rho[1,]
  }
  res$pval  <- pstat$pval[1,]
  res$pcomb <- cstat$pcomb
#  return detail or just final results?
  if( details ) {
    res$pvalcomb   <- cstat$pvalcomb
    res$spatialwtd <- cstat$spatialwtd
    res$pstat      <- pstat
  }
    
  return( res )
}
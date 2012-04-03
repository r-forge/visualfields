loadvfxml <- function( filename, patternMap, typeData = "vf", typeSubject = "pwg", extractionType = c( "average" ) ) {
# loads XML file with visual fields and converts the columns to the correct format
  xmllines <- readLines( filename )

# get all info columns. CARE if VF object changes this part will need to be changed
  xmlobject            <- NULL
  xmlobject$id         <- NA
  xmlobject$tperimetry <- NA
  xmlobject$talgorithm <- NA
  xmlobject$tpattern   <- NA
  xmlobject$tdate      <- NA
  xmlobject$ttime      <- NA
  xmlobject$stype      <- NA
  xmlobject$sage       <- NA
  xmlobject$seye       <- NA
  xmlobject$sbsx       <- NA
  xmlobject$sbsy       <- NA
  xmlobject$sfp        <- NA
  xmlobject$sfn        <- NA
  xmlobject$sfl        <- NA
  xmlobject$sduration  <- NA
  xmlobject$spause     <- NA
  xmlobject <- as.data.frame( xmlobject )
  xmlobject$id         <- xmlitem( "PATIENT_ID", xmllines )
  instrument           <- xmlitem( "INSTRUMENT_NAME", xmllines )
  xmlobject$tperimetry <- switch( instrument,
                                  HFA = "sap" )
  dname <- xmlitem( "DISPLAY_NAME", xmllines )
  xmlobject$talgorithm <- "fullt"
  if( length( grep( "SS", dname ) ) > 0 ) xmlobject$talgorithm <- "sitas"
  if( length( grep( "SF", dname ) ) > 0 ) xmlobject$talgorithm <- "sitaf"
  if( length( grep( "24-2", dname ) ) > 0 ) xmlobject$tpattern <- "p24d2"
  if( length( grep( "30-2", dname ) ) > 0 ) xmlobject$tpattern <- "p30d2"
  if( length( grep( "10-2", dname ) ) > 0 ) xmlobject$tpattern <- "p10d2"
  xmlobject$tdate      <- xmlitem( "VISIT_DATE", xmllines )
  xmlobject$ttime      <- xmlitem( "EXAM_TIME", xmllines )
  xmlobject$stype      <- typeSubject
  xmlobject$sage       <- as.numeric( as.Date( xmlitem( "VISIT_DATE", xmllines ), format = "%Y-%m-%d" ) - as.Date( xmlitem( "BIRTH_DATE", xmllines ), format = "%Y-%m-%d" ) ) / 365
  tfname               <- xmlitem( "IMAGE_FILE_NAME", xmllines )
# SITE 0 is OS and 1 is OD, or so it seems
  site                 <- xmlitem( "SITE", xmllines )
  xmlobject$seye <- switch( site,
                            "0" = "OS",
                            "1" = "OD" )
  xmlobject$sbsx       <- as.numeric( xmlitem( "BLIND_SPOT_X", xmllines ) )
  xmlobject$sbsy       <- xmlitem( "BLIND_SPOT_Y", xmllines )

# getting the subject's false positives, false negatives, and fixation loses is a bit
# more challenging. We need to find the begining and the end of the tag, extract
  fpmethod      <- as.numeric( xmlitem( "FALSE_NEGATIVE_METHOD", xmllines ) )
  if( fpmethod == 1 ) {
    xmlobject$sfp <- as.numeric( xmlitem( "FALSE_POSITIVE_PERCENT", xmllines ) ) / 100
  } else if( fpmethod == 0 ) {
    xmllinesaux   <- xmlblock( "FALSE_NEGATIVES", xmllines )
    xmlobject$sfp <- as.numeric( xmlitem( "ERRORS", xmllinesaux ) ) / as.numeric( xmlitem( "TRIALS", xmllinesaux ) )
  }
  fnmethod      <- as.numeric( xmlitem( "FALSE_POSITIVE_METHOD", xmllines ) )
  if( fnmethod == 1 ) {
    xmlobject$sfn <- as.numeric( xmlitem( "FALSE_NEGATIVE_PERCENT", xmllines ) ) / 100
  } else if( fnmethod == 0 ) {
    xmllinesaux   <- xmlblock( "FALSE_POSITIVES", xmllines )
    xmlobject$sfn <- as.numeric( xmlitem( "ERRORS", xmllinesaux ) ) / as.numeric( xmlitem( "TRIALS", xmllinesaux ) )
  }
  xmllinesaux   <- xmlblock( "FIXATION_CHECK", xmllines )
  xmlobject$sfl <- as.numeric( xmlitem( "ERRORS", xmllinesaux ) ) / as.numeric( xmlitem( "TRIALS", xmllinesaux ) )
  xmlobject$sduration <- xmlitem( "EXAM_DURATION", xmllines )
  xmlobject$spause    <- "00:59:59"

# what do we want?
  if( typeData == "vf" ) {
    xmlvals <- xmlvfval( xmllines, patternMap = patternMap, extractionType = extractionType )
    xmlobject <- cbind( xmlobject, xmlvals )
  } else if( typeData == "td" )  {
    xmlvals <- xmldevval( xmllines, typeData = "td", patternMap = patternMap )
    xmlobject <- cbind( xmlobject, xmlvals )
  } else if( typeData == "pd" )  {
    xmlvals <- xmldevval( xmllines, typeData = "pd", patternMap = patternMap )
    xmlobject <- cbind( xmlobject, xmlvals )
  } else if( typeData == "gi" )  {
    xmlobject$msens  <- NA
    xmlobject$ssens  <- NA
    xmlobject$mtdev  <- xmlitem( "MD", xmllines )
    xmlobject$stdev  <- NA
    xmlobject$mpdev  <- NA
    xmlobject$spdev  <- xmlitem( "PSD", xmllines )
  } else if( typeData == "vfi" ) {
    xmlobject$mvfi  <- xmlitem( "VFI", xmllines )
    xmlobject$svfi  <- NA
  } else if( typeData == "tdp" )  {
    xmlvals <- xmldevval( xmllines, typeData = "tdp", patternMap = patternMap )
    xmlobject <- cbind( xmlobject, xmlvals )
  } else if( typeData == "pdp" )  {
    xmlvals <- xmldevval( xmllines, typeData = "pdp", patternMap = patternMap )
    xmlobject <- cbind( xmlobject, xmlvals )
  } else if( typeData == "gip" )  {
    xmlobject$msens <- NA
    xmlobject$ssens <- NA
    xmlobject$mtdev <- xmlitem( "MD_PROBABILITY", xmllines )
    xmlobject$stdev <- NA
    xmlobject$mpdev <- NA
    xmlobject$spdev <- xmlitem( "PSD_PROBABILITY", xmllines )
  } else if( typeData == "vfip" ) {
    xmlobject$mvfi <- NA
    xmlobject$svfi <- NA
  } else {
    stop("wrong data type to load")
  }

  return( xmlobject )

}
  
setnv <- function( nvtxt = "sapsunyiunv", name = "sapsunyiunv" ) {

  nvinternal        <- eval( parse( text = nvtxt ) ) 
  nvinternal$nvname <- name
  assign("nv", nvinternal, envir = .GlobalEnv)

}

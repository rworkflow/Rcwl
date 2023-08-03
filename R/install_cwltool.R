#' install cwltool
#' 
#' To download and install cwltool using basilisk
#' @export
install_cwltool <- function(){
    cl <- basiliskStart(env_Rcwl)
    basiliskStop(cl)
    return(Sys.which("cwltool"))
}

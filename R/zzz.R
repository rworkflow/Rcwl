#' @importFrom basilisk basiliskStart basiliskStop
.onLoad <- function(libname, pkgname) {
    cl <- basiliskStart(env_Rcwl)
    basiliskStop(cl)
    invisible()
}

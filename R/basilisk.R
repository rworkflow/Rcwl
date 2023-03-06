#' Rcwl conda environment
#'
#' Rcwl conda envrionment to install `cwltool` by basilisk.
#' @importFrom basilisk BasiliskEnvironment
#' @export
env_Rcwl <- basilisk::BasiliskEnvironment("env_Rcwl", pkgname="Rcwl",
                                          packages = c("cwltool==3.1"))

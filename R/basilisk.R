#' Rcwl conda environment
#'
#' Rcwl conda envrionment to install `cwltool` by basilisk.
#' @importFrom basilisk BasiliskEnvironment
#' @export
env_Rcwl <- basilisk::BasiliskEnvironment("env_Rcwl", pkgname="Rcwl",
                                          packages = "python==3.11",
                                          pip = "cwltool==3.1.20230719185429")

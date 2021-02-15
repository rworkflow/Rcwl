#' @importFrom basilisk BasiliskEnvironment
env_Rcwl <- basilisk::BasiliskEnvironment("env_Rcwl", pkgname="basilisk",
                                          packages = "python=3.8",
                                          pip = c("cwltool==3.0.20210124104916"))

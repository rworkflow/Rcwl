#' @importFrom basilisk BasiliskEnvironment
env_Rcwl <- basilisk::BasiliskEnvironment("env_Rcwl", pkgname="Rcwl",
                                          packages = c("cwltool==3.1", "cwl-utils==0.19"))

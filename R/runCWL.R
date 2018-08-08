#' run cwlParam

runCWL <- function(cwl, prefix = tempfile(), cwlRunner = "cwl-runner", ...){
    writeCWL(cwl, prefix = prefix, ...)
    res <- runOSCommand(sys.cmd = cwlRunner,
                        sys.args = paste0(prefix, ".cwl ", prefix, ".yml"))
    return(res)
}

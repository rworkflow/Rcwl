#' run cwlParam
#' Execute a cwlParam object with assigned inputs.
#' @param cwl A `cwlParam` or `cwlStepParam` object.
#' @param prefix The prefix of `cwl` and `yml` file to write.
#' @param cwlRunner The path to the `cwltool` or `cwl-runner`.
#' @param Args The arguments for `cwltool` or `cwl-runner`. For example, "--debug" can work with `cwltool` to show debug information.
#' @param stdout standard output from `system2`.
#' @param stderr standard error from `system2`.
#' @param ... The other options from `writeCWL` and `system2`.
#' @export
runCWL <- function(cwl, prefix = tempfile(), cwlRunner = "cwltool", Args = character(), stdout = TRUE, stderr = TRUE, ...){
    if(length(unlist(.cwl2yml(cwl))) == 0) stop("Inputs are not defined")
    writeCWL(cwl, prefix = prefix, ...)
    res <- system2(cwlRunner,
                   args = paste0(Args, " ", prefix, ".cwl ", prefix, ".yml"),
                   stdout = stdout, stderr = stderr, ...)
    ##return(res)
    message(tail(res, 1))

    if(stderr == TRUE) {
        if(cwlClass(cwl) == "CommandLineTool"){
            idx <- grep("^\\[job", res)
            command <- res[idx[1] : (idx[2]-1)]
        }else{
            command <- NULL
        }
        if(!any(grepl("path", res)) & any(grepl("location", res))){
            path <- read.table(textConnection(res[grep("location", res)]), stringsAsFactors = FALSE)[,3]
        }else if(any(grepl("path", res))){
            path <- read.table(textConnection(res[grep("path", res)]), stringsAsFactors = FALSE)[,3]
        }else{
            path <- NULL
        }
        SimpleList(command = command,
                   output = path,
                   logs = res)
    }else{
        res
    }
}

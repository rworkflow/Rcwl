#' run cwlParam
#' @export
runCWL <- function(cwl, prefix = tempfile(), cwlRunner = "cwl-runner", Args = character(), stdout = TRUE, stderr = TRUE, ...){
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

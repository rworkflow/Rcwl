#' run cwlParam

runCWL <- function(cwl, prefix = tempfile(), cwlRunner = "cwl-runner", ...){
    writeCWL(cwl, prefix = prefix, ...)
    res <- system2(cwlRunner,
                   args = paste0(prefix, ".cwl ", prefix, ".yml"),
                   stdout = TRUE, stderr = TRUE)
    ##return(res)
    idx <- grep("^\\[job", res)
    command <- res[idx[1] : (idx[2]-1)]
    if(!any(grepl("path", res)) & any(grepl("location", res))){
        path <- read.table(textConnection(res[grep("location", res)]), stringsAsFactors = FALSE)[1,3]
    }else if(any(grepl("path", res))){
        path <- read.table(textConnection(res[grep("path", res)]), stringsAsFactors = FALSE)[1,3]
    }else{
        path <- NA
    }
    message(tail(res, 1))
    SimpleList(command = command,
               output = path,
               logs = res)
}

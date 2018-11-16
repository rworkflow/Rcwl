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


runFun <- function(idx, cwl, wdir, inputList, paramList = list(), ...){
    library(Rcwl)
    stopifnot(all(names(inputList) %in% names(inputs(cwl))))
    ## change work directory
    sname <- names(inputList[[1]])
    wdir <- file.path(wdir, sname[idx])
    dir.create(wdir, showWarnings = FALSE, recursive = TRUE)
    setwd(wdir)
    ## assign inputs
    for(i in 1:length(inputList)){
        cwl <- .assignInput(cwl, names(inputList)[i], inputList[[i]][[idx]])
    }
    if(length(paramList) > 0){
        for(j in 1:length(paramList)){
            cwl <- .assignInput(cwl, names(paramList)[j], paramList[[j]])
        }
    }
    runCWL(cwl, ...)
}

#' run CWL with batchtools
#' @param cwl A `cwlParam` or `cwlStepParam` object.
#' @param wdir Directory to output results
#' @param inputList An input list to run in parallel. The list names must be in the inputs of cwl. Jobs will be submitted in parallel for each element in the list. The output directory of each job will be made using the name of each element under the `wdir`.
#' @param paramList A parameter list for the cwl. The list names must be in the inputs of cwl. 
#' @param BPoptions The options for `BatchtoolsParam`.
#' @param ... The options from runCWL.
#' @import BiocParallel
#' @export
runCWLBatch <- function(cwl, wdir = getwd(), inputList, paramList = list(),
                        BPoptions = list(), ...){
    nsample <- lengths(inputList)[1]
    ##param <- BatchtoolsParam(workers = nsample, cluster = cluster, ...)
    param <- do.call(BatchtoolsParam, c(list(workers = nsample), BPoptions))
    bptry(bplapply(seq(nsample), runFun, BPPARAM = param,
                   cwl = cwl, wdir = wdir,
                   inputList = inputList,
                   paramList = paramList, ...))
}

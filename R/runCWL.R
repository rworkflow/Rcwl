#' run cwlParam
#' 
#' Execute a cwlParam object with assigned inputs.
#' @param cwl A `cwlParam` or `cwlStepParam` object.
#' @param prefix The prefix of `cwl` and `yml` file to write.
#' @param cwlRunner The path to the `cwltool` or `cwl-runner`. If not
#'     exists, the cwltool package will be installed by `reticulate`.
#' @param cwlTemp Whether to keep temporary files. If true, all
#'     temporary files will be kept in a "temp" folder of current
#'     output directory.
#' @param Args The arguments for `cwltool` or `cwl-runner`. For
#'     example, "--debug" can work with `cwltool` to show debug
#'     information.
#' @param stdout standard output from `system2`.
#' @param stderr standard error from `system2`. By setting it to "",
#'     the detailed running logs will return directly.
#' @param noDocker Whether to disable docker.
#' @param ... The other options from `writeCWL` and `system2`.
#' @import reticulate
#' @export
#' @return A list of outputs from tools and logs from cwltool.
#' @examples
#' input1 <- InputParam(id = "sth")
#' echo <- cwlParam(baseCommand = "echo",
#'                  inputs = InputParamList(input1))
#' echo$sth <- "Hello World!"
#' ## res <- runCWL(echo)
runCWL <- function(cwl, prefix = tempfile(), cwlRunner = "cwltool",
                   cwlTemp = FALSE, Args = character(), stdout = TRUE,
                   stderr = TRUE, noDocker = FALSE, ...){
    if(length(unlist(.cwl2yml(cwl))) == 0) stop("Inputs are not defined")
    writeCWL(cwl, prefix = prefix, noDocker = noDocker, ...)

    ## check cwltool
    ext <- suppressWarnings(system(paste("which", cwlRunner),
                                   intern = TRUE))
    if(cwlRunner != "cwltool" & cwlRunner != "cwl-runner"){
        if(length(ext)==0) stop(cwlRunner, " not exist!")
    }else if(length(ext)==0){
        inres <- tryCatch({
            reticulate:::virtualenv_config()
        }, error = function(e)e)
        if(is(inres, "error")){
            if(grepl("virtualenv", inres)){
                message("Install virtualenv ...")
                system("pip install --user virtualenv")
            }
        }

        config <- reticulate:::virtualenv_config() 
        virtualenv_path <- file.path(config$root, "r-reticulate")
        cr <- path.expand(file.path(virtualenv_path, 
                                    "bin", "cwltool"))
        if(file.exists(cr)){
            message("Find cwltool: ", cr)
        }else{
            message("Install cwltool ...")
            py_install("cwltool",
                       envname = "r-reticulate",
                       method = "virtualenv")
        }
        cwlRunner <- cr
    }
    
    if(cwlTemp){
        Args <- paste("--tmp-outdir-prefix temp/", Args)
    }
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
            path <- read.table(textConnection(res[grep('\"location\":', res)]),
                               stringsAsFactors = FALSE)[,3]
        }else if(any(grepl("path", res))){
            path <- read.table(textConnection(res[grep('\"path\":', res)]),
                               stringsAsFactors = FALSE)[,3]
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
    ## assign inputs
    for(i in seq_along(inputList)){
        input1 <- inputList[[i]][[idx]]
        cwl <- .assignInput(cwl, names(inputList)[i], input1)
    }
    if(length(paramList) > 0){
        for(j in seq_along(paramList)){
            cwl <- .assignInput(cwl, names(paramList)[j], paramList[[j]])
        }
    }
    setwd(wdir)
    runCWL(cwl, ...)
}

#' run CWL with batchtools
#' 
#' @param cwl A `cwlParam` or `cwlStepParam` object.
#' @param wdir Directory to output results
#' @param inputList An input list to run in parallel. The list names
#'     must be in the inputs of cwl. Jobs will be submitted in
#'     parallel for each element in the list. The output directory of
#'     each job will be made using the name of each element under the
#'     `wdir`.
#' @param paramList A parameter list for the cwl. The list names must
#'     be in the inputs of cwl.
#' @param BPPARAM The options for `BiocParallelParam`.
#' @param ... The options from runCWL.
#' @import BiocParallel
#' @import batchtools
#' @export
#' @return Results from computing nodes and logs from cwltool.
runCWLBatch <- function(cwl, wdir = getwd(), inputList, paramList = list(),
                        BPPARAM = BatchtoolsParam(
                            workers = lengths(inputList)[1]), ...){
    nsample <- lengths(inputList)[1]
    ##param <- do.call(BatchtoolsParam, c(list(workers = nsample), BPoptions))
    if(!dir.exists(wdir)) dir.create(wdir, recursive = TRUE)
    bptry(bplapply(seq(nsample), runFun, BPPARAM = BPPARAM,
                   cwl = cwl, wdir = normalizePath(wdir),
                   inputList = inputList,
                   paramList = paramList, ...))
}

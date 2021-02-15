#' run cwlProcess
#' 
#' Execute a cwlProcess object with assigned inputs.
#' @param cwl A `cwlProcess` or `cwlWorkflow` object.
#' @param cwlRunner The path to the `cwltool` or `cwl-runner`. If not
#'     exists, the cwltool package will be installed by `reticulate`.
#' @param outdir Output directory, default is current working
#'     directory.
#' @param cwlTemp File path to keep intermediate files. If a directory
#'     path is given, the intermediate files will be kept in the
#'     directory. Default is NULL to remove all intermediate files.
#' @param cwlArgs The arguments for `cwltool` or `cwl-runner`. For
#'     example, "--debug" can work with `cwltool` to show debug
#'     information.
#' @param stdout standard output from `system2`.
#' @param stderr standard error from `system2`. By setting it to "",
#'     the detailed running logs will return directly.
#' @param showLog Whether to show log details to standard
#'     out. i.e. stderr = "".
#' @param docker Whether to use docker, or "sigularity" if use
#'     Singularity runtime to run container.
#' @param yml_prefix The prefix of `.cwl` and `.yml` files that are to
#'     be internally executed.
#' @param yml_outdir The output directory for the `.cwl` and `.yml`
#'     files.
#' @param ... The other options from `writeCWL` and `system2`.
#' @importFrom basilisk basiliskStart basiliskStop
#' @export
#' @return A list of outputs from tools and logs from cwltool.
#' @examples
#' input1 <- InputParam(id = "sth")
#' echo <- cwlProcess(baseCommand = "echo",
#'                  inputs = InputParamList(input1))
#' echo$sth <- "Hello World!"
#' ## res <- runCWL(echo)
runCWL <- function(cwl, cwlRunner = "cwltool",
                   outdir = ".", cwlTemp = NULL, cwlArgs = character(),
                   stdout = TRUE, stderr = TRUE, showLog = FALSE,
                   docker = TRUE,
                   yml_prefix = deparse(substitute(cwl)),
                   yml_outdir = tempfile(), ...){
    ## check missing inputs
    yml0 <- .removeEmpty(.cwl2yml(cwl))
    if(length(yml0) < length(inputs(cwl))){
        mis <- setdiff(names(inputs(cwl)), names(yml0))
        ifReq <- rep(TRUE, length(mis))
        for(i in seq_along(mis)){
            type1 <- inputs(cwl)[[mis[i]]]@type
            if(is.character(type1) | is.list(type1)){
                ifReq[i] <- !any(grepl("\\?", type1))
            }
        }
        mis <- mis[ifReq]
        stop("Input parameter(s): ", paste(mis, collapse = ", "), " not assigned.")
    }
    if(docker == "singularity"){
        cwlArgs <- paste("--singularity", cwlArgs)
        docker <- TRUE
    }else if(docker == "udocker"){
        cwlArgs <- paste("--user-space-docker-cmd=udocker", cwlArgs)
        docker <- TRUE
    }
    if (!dir.exists(yml_outdir)) dir.create(yml_outdir)
    writeCWL(cwl, prefix = yml_prefix, outdir = yml_outdir, docker = docker, ...)

    ## ## check cwltool
    ## ext <- suppressWarnings(system(paste("which", cwlRunner),
    ##                                intern = TRUE))
    ## if(length(ext)==0){
    ##     stop(cwlRunner, " is not found, ",
    ##         "Please install cwltool first!\n",
    ##          "https://github.com/common-workflow-language/cwltool#install")
    ## }
    if(!file.exists(Sys.which(cwlRunner))){
        cl <- basiliskStart(env_Rcwl)
        basiliskStop(cl)
    }
    if(!is.null(cwlTemp)){
        cwlArgs <- paste("--tmp-outdir-prefix", cwlTemp, cwlArgs)
    }
    if(showLog){
        stderr <- ""
    }
    res <- system2(cwlRunner,
                   args = paste0("--outdir ", outdir, " ", cwlArgs, " ",
                                 file.path(yml_outdir, paste0(yml_prefix, ".cwl ")),
                                 file.path(yml_outdir, paste0(yml_prefix, ".yml"))),
                   stdout = stdout, stderr = stderr, ...)
    ##return(res)
    message(tail(res, 1))
    if(!any(grepl("path", res)) & any(grepl("location", res))){
        path <- read.table(textConnection(res[grep('\"location\":', res)]),
                           stringsAsFactors = FALSE)[,3]
    }else if(any(grepl("path", res))){
        path <- read.table(textConnection(res[grep('\"path\":', res)]),
                           stringsAsFactors = FALSE)[,3]
    }else{
        path <- NULL
    }
    if(stderr == TRUE) {
        if(cwlClass(cwl) == "CommandLineTool"){
            idx <- grep("\\[job", res)
            command <- res[idx[1] : (idx[2]-1)]
        }else{
            command <- NULL
        }
    }else{
        command <- NULL
    }
    SimpleList(command = command,
               output = path,
               logs = res)
}


runFun <- function(idx, cwl, outdir, inputList, paramList = list(), ...){
    library(Rcwl)
    idx <- names(inputList) %in% names(inputs(cwl))
    if(!all(idx)){
        warning(paste(names(inputList)[!idx], collapse = ", "),
                " oare not assigned")
    }
    ## change output directory
    sname <- names(inputList[[1]])
    outdir <- file.path(outdir, sname[idx])
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
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
    runCWL(cwl, outdir = outdir, ...)
}

#' run CWL with batchtools
#' 
#' @param cwl A `cwlProcess` or `cwlWorkflow` object.
#' @param outdir Directory to output results
#' @param inputList An input list to run in parallel. The list names
#'     must be in the inputs of cwl. Jobs will be submitted in
#'     parallel for each element in the list. The output directory of
#'     each job will be made using the name of each element under the
#'     `outdir`.
#' @param paramList A parameter list for the cwl. The list names must
#'     be in the inputs of cwl.
#' @param BPPARAM The options for `BiocParallelParam`.
#' @param ... The options from runCWL.
#' @import BiocParallel
#' @import batchtools
#' @export
#' @return Results from computing nodes and logs from cwltool.
runCWLBatch <- function(cwl, outdir = getwd(), inputList, paramList = list(),
                        BPPARAM = BatchtoolsParam(
                            workers = lengths(inputList)[1]), ...){
    nsample <- lengths(inputList)[1]
    if(!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
    bptry(bplapply(seq(nsample), runFun, BPPARAM = BPPARAM,
                   cwl = cwl, outdir = normalizePath(outdir),
                   inputList = inputList,
                   paramList = paramList, ...))
}

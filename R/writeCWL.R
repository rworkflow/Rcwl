.rmDList <- function(reqList){
    if(length(reqList) > 0){
        cs <- unlist(lapply(reqList, function(x)x$class))
        if("DockerRequirement" %in% names(reqList)){
            reqList <- reqList[-match("DockerRequirement", names(reqList))]
        }else if( "DockerRequirement" %in% cs){
            reqList <- reqList[-match("DockerRequirement", cs)]
        }
    }
    return(reqList)
}

.rmDocker <- function(cwl){
    requirements(cwl) <- .rmDList(requirements(cwl))
    hints(cwl) <- .rmDList(hints(cwl))
    return(cwl)
}

## Write R function into Rscript
#' @importFrom R.utils commandArgs
#' @importFrom codetools findGlobals
#' 
writeFun <- function(cwl, prefix, outdir, libPaths = TRUE){
    Fname <- ifelse(is.null(prefix), basename(tempfile()), prefix)
    if(length(cwl@id) > 0){
        file <- file.path(outdir, paste0(cwl@id, ".R"))
    }else{        
        file <- file.path(outdir, paste0(Fname, ".R"))
    }
    funName <- sub(".R", "", basename(file))
    assign(funName, baseCommand(cwl))
    types <- lapply(inputs(cwl), function(x)x@type)
    comArg <- c("suppressPackageStartupMessages(library(R.utils))",                
                "args <- commandArgs(trailingOnly = TRUE, asValues = TRUE)")
    ## add user libPaths
    if(libPaths){
        libs <- .libPaths()
        comArg <- c(paste0(".libPaths('", libs, "')"), comArg)
    }
    write(comArg, file = file)

    for(i in seq_along(types)){
        tn <- names(types)[i]
        if(types[[i]] == "int"){
            write(paste0("args[[\"", tn, "\"]] <- as.integer(args[[\"", tn, "\"]])"),
                  file = file, append = TRUE)
        }else if(types[[i]] %in% c("long", "float", "double")){
            write(paste0("args[[\"", tn, "\"]] <- as.numeric(args[[\"", tn, "\"]])"),
                  file = file, append = TRUE)
        }
    }
    ## didn't work
    ff <- findGlobals(baseCommand(cwl))
    ff <- ff[!grepl("package|namespace", sapply(ff, find))]
    if(length(ff) > 0){
        sapply(ff, dump, file = file, append = TRUE)
    }
    dump(funName, file = file, append = TRUE)
    write(paste0("do.call(", funName, ", args)"),
          file = file, append = TRUE)
    return(file)
}

cwlToList <- function(cwl, docker = TRUE, prefix, outdir){
    stopifnot(is(cwl, "cwlProcess"))
    if(!docker) cwl <- .rmDocker(cwl)
    if(is(baseCommand(cwl), "function")){
        rfile <- writeFun(cwl, prefix, outdir)
        bc <- c("Rscript", rfile)
    }else{
        bc <- baseCommand(cwl)
    }
    CL <- list(cwlVersion = cwlVersion(cwl),
               class = cwlClass(cwl),
               baseCommand = bc,
               requirements = cwl@requirements,
               hints = cwl@hints,
               arguments = cwl@arguments,
               id = cwl@id,
               label = cwl@label,
               inputs = as.listInputs(inputs(cwl)),
               outputs = as.listOutputs(outputs(cwl)),
               stdout = cwl@stdout,
               expression = cwl@expression)
    CL <- c(CL, cwl@extensions)
    ## CL$requirements <- .removeEmpty(CL$requirements)
    CL <- .removeEmpty(CL)
    if(cwlClass(cwl) == "Workflow"){
        CL <- c(CL, list(steps = as.listSteps(cwl@steps)))
        ## remove inputBinding
        for(i in seq(CL$inputs)){
            CL$inputs[[i]]$inputBinding <- NULL
        }
    }else if (cwlClass(cwl) == "ExpressionTool") {
        for(i in seq(CL$inputs)){
            CL$inputs[[i]]$inputBinding <- NULL
        }
    }
    if(is.null(CL$outputs)) CL$outputs <- list()
    return(CL)
}

## Nested steps
allRun <- function(cwl){
    Steps <- steps(cwl)
    Run <- c()
    for(i in seq(Steps)){
        nm1 <- names(Steps)[i]
        run1 <- Steps[[i]]@run

        if(is(run1, "cwlProcess") & !is(run1, "cwlWorkflow")){
            nn <- names(Run)
            Run <- c(Run, run1)
            names(Run) <- c(nn, nm1)
        }else if(is(run1, "cwlWorkflow")){
            ## record cwlWorkflow
            nn <- names(Run)
            Run <- c(Run, run1)
            names(Run) <- c(nn, nm1)
            ## recursive
            Run <- c(Run, allRun(run1))
        }
    }
    return(Run)
}

#' Write CWL
#' 
#' write `cwlProcess` to cwl and yml.
#' @param cwl A `cwlProcess` or `cwlWorkflow` object.
#' @param prefix The prefix of `.cwl` and `.yml` files to be generated.
#' @param outdir The output directory for the `.cwl` and `.yml` files. 
#' @param docker Whether to use docker.
#' @param libPaths Whether to add local R libaray paths to R script.
#' @param ... Other options from `yaml::write_yaml`.
#' @import yaml
#' @export
#' @return A CWL file and A YML file.
#' @examples
#' input1 <- InputParam(id = "sth")
#' echo <- cwlProcess(baseCommand = "echo",
#'                  inputs = InputParamList(input1))
#' writeCWL(echo)
writeCWL <- function(cwl, prefix = deparse(substitute(cwl)),
                     outdir = tempfile(),
                     docker = TRUE, libPaths = TRUE, ...){
    if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
    stopifnot(is(cwl, "cwlProcess"))
    ## logical to true/false
    handlers  <-  list(
        logical = function(x) {
            result <- ifelse(x, "true", "false")
            class(result) <- "verbatim"
            return(result)
        }
    )
    yml <- .removeEmpty(.cwl2yml(cwl))

    .R2cwl <- function(x, prefix, outdir){
        rfile <- writeFun(x, prefix, outdir = outdir, libPaths)
        baseCommand(x) <- "Rscript"
        requirements(x) <- c(list(requireRscript(rfile)), requirements(x))
        arguments(x) <- c(list(basename(rfile)), arguments(x))
        return(x)
    }
    
    if(cwlClass(cwl) == "Workflow") {
        Runs <- allRun(cwl)
        lapply(seq(Runs), function(i){
            cfile <- paste0(file.path(outdir, 
                                      names(Runs)[i]), ".cwl")
            if (is(baseCommand(Runs[[i]]), "function")) {
                Runs[[i]] <- .R2cwl(Runs[[i]], names(Runs)[i], outdir)
            }
            write_yaml(cwlToList(Runs[[i]], docker,
                                 prefix = sub(".cwl", "", basename(cfile)),
                                 outdir),
                       file = cfile,
                       handlers = handlers, ...)
        })
        
        ## NOTE: Now nested pipelines and related tools are all written
        ## in the same directory. Future conflict may rise when  nested
        ## pipelines are using the same tool but with different options,
        ## there will be different "id" for each tool. Or we can later add
        ## a prefix of the pipeline name on that specific tool. 2/12/2012
        
        ## for(i in seq(cList$steps)){
        ##     if(!grepl("^/", cList$steps[[i]]$run)){
        ##         cList$steps[[i]]$run <- file.path(outdir,
        ##                                           cList$steps[[i]]$run)
        ##     }
    }else if(is(baseCommand(cwl), "function")){
        cwl <- .R2cwl(cwl, prefix, outdir)
    }
    cList <- cwlToList(cwl, docker, prefix, outdir)
    
    cwlout <- file.path(outdir, paste0(prefix, ".cwl"))
    ymlout <- file.path(outdir, paste0(prefix, ".yml"))
    write_yaml(cList, file = cwlout, handlers = handlers, ...)
    write_yaml(yml, file = ymlout, handlers = handlers, ...)
    return(c(cwlout = cwlout, ymlout = ymlout))
}

.cwl2yml <- function(cwl){
    lapply(inputs(cwl), function(x) {

        if(length(x@value) > 0) {
            v <- x@value
        }else if(length(x@default) > 0){
            v <- x@default
        }else{
            v <- NULL
        }
        if(is(x@type, "InputArrayParam")){
            Type <- x@type@items
        }else{
            Type <- x@type
        }        
        if(is(v, "character") && Type == "int"){
            v <- as.integer(v)
        }else if(is(v, "character") && Type == "boolean"){
            v <- as.logical(v)
        }
        if(length(x@format)!=0){
            v <- c(format = x@format, v)
        }
        v
    })
}

.slot2list <- function(x) {
    mapply(function(y) slot(x, y),
           slotNames(class(x)),
           SIMPLIFY = FALSE)
}

.removeEmpty <- function(L) {
    L <- L[L != ""]
    L[lengths(L) > 0]
}

as.listInputs <- function(Inputs){
    alist <- lapply(Inputs, .slot2list)

    for(i in seq(alist)){
        if(is(alist[[i]]$type, "InputArrayParam")){
            atype <- .slot2list(alist[[i]]$type)
            atype <- .removeEmpty(atype)
            atype$inputBinding <- .removeEmpty(atype$inputBinding)
            alist[[i]]$type <- atype
        }
        
        if(alist[[i]]$inputBinding$position == 0){
            alist[[i]]$inputBinding$position <- NULL
        }else if(alist[[i]]$inputBinding$position < 0){
            alist[[i]]$inputBinding <- NULL
        }

        alist[[i]]$inputBinding <- .removeEmpty(alist[[i]]$inputBinding)
        alist[[i]]$value <- NULL
        alist[[i]]$id <- NULL
        alist[[i]] <- .removeEmpty(alist[[i]])
    }
    return(alist)
}

as.listOutputs <- function(Outputs){
    olist <- lapply(Outputs, .slot2list)
    for(i in seq(olist)){
        olist[[i]]$id <- NULL
        if(is(olist[[i]]$type, "OutputArrayParam")){
            otype <- .removeEmpty(.slot2list(olist[[i]]$type))
            otype$outputBinding <- .removeEmpty(otype$outputBinding)
            olist[[i]]$type <- otype
        }
        olist[[i]]$outputBinding <- .removeEmpty(olist[[i]]$outputBinding)
        olist[[i]] <- .removeEmpty(olist[[i]])        
    }
    return(olist)
}


as.listSteps <- function(Steps){
    slist <- lapply(Steps, function(st) {
        sIns <- lapply(st@In, function(x) {
            ilist1 <- .slot2list(x)
            ilist1 <- ilist1[lengths(ilist1) > 0]
            ilist1$id <- NULL
            if(all(names(ilist1) == "source")){
                ilist1 <- ilist1$source
            }
            ilist1
        })

        if(is(st@run, "cwlProcess")){
            run <- paste0(st@id, ".cwl")
        }else{
            run <- st@run
        }
        .removeEmpty(
            list(run = run,
                 "in" = sIns,
                 out = st@Out,
                 scatter = st@scatter,
                 scatterMethod = st@scatterMethod,
                 label = st@label,
                 doc = st@doc,
                 requirements = st@requirements,
                 hints = st@hints,
                 when = st@when)
        )
    })
    return(slist)
}

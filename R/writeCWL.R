
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

cwlToList <- function(cwl, noDocker){
    stopifnot(is(cwl, "cwlParam"))
    if(noDocker) cwl <- .rmDocker(cwl)
    CL <- list(cwlVersion = cwlVersion(cwl),
               class = cwlClass(cwl),
               baseCommand = baseCommand(cwl),
               requirements = cwl@requirements,
               hints = cwl@hints,
               arguments = cwl@arguments,
               id = cwl@id,
               label = cwl@label,
               inputs = as.listInputs(inputs(cwl)),
               outputs = as.listOutputs(outputs(cwl)),
               stdout = cwl@stdout)
    ##CL <- CL[lengths(CL)>0]
    CL$requirements <- .removeEmpty(CL$requirements)
    CL <- .removeEmpty(CL)
    if(cwlClass(cwl) == "Workflow"){
        CL <- c(CL, list(steps = as.listSteps(cwl@steps@steps)))

        ## remove inputBinding
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

        if(class(run1) == "cwlParam"){
            nn <- names(Run)
            Run <- c(Run, run1)
            names(Run) <- c(nn, nm1)
        }else if(class(run1) == "cwlStepParam"){
            ## record cwlStepParam
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
#' write `cwlParam` to cwl and yml.
#' @param cwl A `cwlParam` or `cwlStepParam` object.
#' @param prefix The prefix of `cwl` and `yml` file to write.
#' @param noDocker Whether to disable docker. 
#' @param ... Other options from `yaml::write_yaml`.
#' @importFrom yaml write_yaml
#' @export
writeCWL <- function(cwl, prefix, noDocker = FALSE, ...){
    stopifnot(is(cwl, "cwlParam"))
    ## logical to true/false
    handlers  <-  list(
        logical = function(x) {
            result <- ifelse(x, "true", "false")
            class(result) <- "verbatim"
            return(result)
        }
    )
    yml <- .removeEmpty(.cwl2yml(cwl))

    if(cwlClass(cwl) == "Workflow") {
        Runs <- allRun(cwl)
        lapply(seq(Runs), function(i){
            write_yaml(cwlToList(Runs[[i]], noDocker),
                       file = paste0(file.path(dirname(prefix),
                                               names(Runs)[[i]]), ".cwl"),
                       handlers = handlers, ...)
        })
        
        cList <- cwlToList(cwl, noDocker)
        for(i in seq(cList$steps)){
            if(!grepl("^/", cList$steps[[i]]$run)){
                cList$steps[[i]]$run <- file.path(dirname(prefix), cList$steps[[i]]$run)
            }
        }
    }else{
        cList <- cwlToList(cwl, noDocker)
    }
    write_yaml(cList, file = paste0(prefix, ".cwl"), handlers = handlers, ...)
    write_yaml(yml, file = paste0(prefix, ".yml"), handlers = handlers, ...)
}

.cwl2yml <- function(cwl){
    lapply(inputs(cwl), function(x) {
        ## ## remove empty path
        ## if(grepl("File\\[\\]", x@type)){
        ##     for(i in seq(x@value)){
        ##         if(x@value[[i]]$path == ""){
        ##             x@value[[i]] <- NULL
        ##         }
        ##     }
        ## }else if(x@type=="File?"){
        ##     if(x@value$path == ""){
        ##         x@value <- NULL
        ##     }
        ## }

        if(length(x@value) > 0) {
            v <- x@value
        }else if(length(x@default) > 0){
            v <- x@default
        }else{
            v <- NULL
        }
        if(class(x@type)=="InputArrayParam"){
            Type <- x@type@items
        }else{
            Type <- x@type
        }        
        if(is(v, "character") && Type == "int"){
            v <- as.integer(v)
        }else if(is(v, "character") && Type == "boolean"){
            v <- as.logical(v)
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
        }
        ## if(alist[[i]]$inputBinding$separate){
        ##     alist[[i]]$inputBinding$separate <- NULL
        ## }
        alist[[i]]$inputBinding <- .removeEmpty(alist[[i]]$inputBinding)
        alist[[i]]$value <- NULL
        alist[[i]]$id <- NULL
        ## alist[[i]]$default <- NULL
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
        sIns <- lapply(st@In@Ins, function(x) {
            ilist1 <- .slot2list(x)
            ilist1 <- ilist1[lengths(ilist1) > 0]
            ilist1$id <- NULL
            if(all(names(ilist1) == "source")){
                ilist1 <- ilist1$source
            }
            ilist1
        })

        if(is(st@run, "cwlParam")){
            run <- paste0(st@id, ".cwl")
        }else{
            run <- st@run
        }
        .removeEmpty(
            list(run = run,
                 "in" = sIns,
                 out = st@Out,
                 scatter = st@scatter,
                 scatterMethod = st@scatterMethod)
        )
    })
    return(slist)
}

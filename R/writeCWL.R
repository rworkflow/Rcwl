#' Write CWL
cwlToList <- function(cwl){
    stopifnot(is(cwl, "cwlParam"))
    
    CL <- list(cwlVersion = cwlVersion(cwl),
               class = cwlClass(cwl),
               baseCommand = baseCommand(cwl),
               requirements = cwl@requirements,
               hints = cwl@hints,
               arguments = cwl@arguments,
               inputs = as.listInputs(inputs(cwl)),
               outputs = as.listOutputs(outputs(cwl)),
               stdout = cwl@stdout)
    CL <- CL[lengths(CL)>0]
    if(cwlClass(cwl) == "Workflow"){
        CL <- c(CL, list(steps = as.listSteps(cwl@steps@steps)))
    }
    return(CL)
}

#' write cwlParam to cwl and yml
#' @importFrom yaml write_yaml
writeCWL <- function(cwl, prefix, ...){
    stopifnot(is(cwl, "cwlParam"))
    ## logical to true/false
    handlers  <-  list(
        logical = function(x) {
            result <- ifelse(x, "true", "false")
            class(result) <- "verbatim"
            return(result)
        }
    )
    yml <- lapply(inputs(cwl), function(x) x@value)

    if(cwlClass(cwl) == "Workflow") {
        Steps <- cwl@steps@steps
        lapply(seq(Steps), function(i) {
            run1 <- Steps[[i]]@run
            write_yaml(cwlToList(run1),
                       file = paste0(file.path(dirname(prefix), names(Steps)[[i]]), ".cwl"),
                       handlers = handlers, ...)
        })

        cList <- cwlToList(cwl)
        for(i in seq(cList$steps)){
            cList$steps[[i]]$run <- file.path(dirname(prefix), cList$steps[[i]]$run)
        }
    }else{
        cList <- cwlToList(cwl)
    }
    write_yaml(cList, file = paste0(prefix, ".cwl"), handlers = handlers, ...)
    ## ## fix "["
    ## cfile <- readLines(paste0(prefix, ".cwl"))
    ## cfile <- gsub("'\\[", "\\[", cfile)
    ## cfile <- gsub("\\]'", "\\]", cfile)
    ## writeLines(cfile, paste0(prefix, ".cwl"))
    
    write_yaml(yml, file = paste0(prefix, ".yml"), handlers = handlers, ...)
}


#' convert Inputs/Outputs to a list
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
    ## alist <- lapply(Inputs, function(x){
    ##     mapply(function(y) slot(x, y),
    ##     slotNames(class(x)),
    ##     SIMPLIFY = FALSE)
    ## })
    alist <- lapply(Inputs, .slot2list)
    for(i in seq(alist)){
        ##alist[[i]][alist[[i]] == ""] <- NULL
        if(alist[[i]]$inputBinding$position == 0){
            alist[[i]]$inputBinding$position <- NULL
        }
        ## if(alist[[i]]$inputBinding$prefix == ""){
        ##     alist[[i]]$inputBinding$prefix <- NULL
        ##     ##alist[[i]]$inputBinding$separate <- NULL
        ## }
        ## alist[[i]]$inputBinding <- alist[[i]]$inputBinding[lengths(alist[[i]]$inputBinding) > 0]
        alist[[i]]$inputBinding <- .removeEmpty(alist[[i]]$inputBinding)
        alist[[i]]$value <- NULL
        alist[[i]]$id <- NULL
        alist[[i]] <- .removeEmpty(alist[[i]])
        ##alist[[i]] <- alist[[i]][lengths(alist[[i]]) > 0]
    }
    return(alist)
}

## as.listOutputs <- function(Outputs){
##     olist <- mapply(function(x) slot(Outputs, x),
##                     slotNames(class(Outputs)),
##                     SIMPLIFY = FALSE)
##     id <- olist$id
##     olist[olist == ""] <- NULL

##     olist$id <- NULL
##     if(olist$streamable == "false") {
##         olist$streamable <- NULL
##     }
    
##     if(olist$type == "stdout"){
##         olist$id <- NULL
##         olist$streamable <- NULL
##         olist$outputBinding <- NULL
##     }
##     olist <- list(olist)
##     names(olist) <- id
##     return(olist)
## }

as.listOutputs <- function(Outputs){
    ## olist <- lapply(Outputs, function(x){
    ##     mapply(function(y) slot(x, y),
    ##     slotNames(class(x)),
    ##     SIMPLIFY = FALSE)
    ## })
    olist <- lapply(Outputs, .slot2list)
    for(i in seq(olist)){
        ##olist[[i]][olist[[i]] == ""] <- NULL
        olist[[i]]$id <- NULL

        if(olist[[i]]$streamable == "false") {
            olist[[i]]$streamable <- NULL
        }
        
        if(olist[[i]]$type == "stdout"){
            olist[[i]]$streamable <- NULL
            olist[[i]]$outputBinding <- NULL
        }

        ## if(olist[[i]]$outputBinding$glob == "") {
        ##     olist[[i]]$outputBinding <- NULL
        ## }
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
    
        list(run = paste0(st@id, ".cwl"),
             "in" = sIns,
             out = st@Out)
    })
    return(slist)
}

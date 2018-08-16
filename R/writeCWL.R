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

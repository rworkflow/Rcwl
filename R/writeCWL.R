#' Write CWL
cwlToList <- function(cwl){
    stopifnot(is(cwl, "cwlParam"))
    list(cwlVersion = cwlVersion(cwl),
         class = cwlClass(cwl),
         baseCommand = baseCommand(cwl),
         inputs = as.listInputs(inputs(cwl)),
         outputs = as.listOutputs(outputs(cwl)))
}

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

    write_yaml(cwlToList(cwl), file = paste0(prefix, ".cwl"), handlers = handlers, ...)
    write_yaml(yml, file = paste0(prefix, ".yml"), handlers = handlers, ...)
}


#' convert Inputs/Outputs to a list
as.listInputs <- function(Inputs){
    alist <- lapply(Inputs, function(x){
        mapply(function(y) slot(x, y),
        slotNames(class(x)),
        SIMPLIFY = FALSE)
    })
    
    for(i in seq(alist)){
        alist[[i]][alist[[i]] == ""] <- NULL
        if(alist[[i]]$inputBinding$position == 0){
            alist[[i]]$inputBinding$position <- NULL
        }
        alist[[i]]$value <- NULL
    }
    return(alist)
}

as.listOutputs <- function(Outputs){
    olist <- mapply(function(x) slot(Outputs, x),
                    slotNames(class(Outputs)),
                    SIMPLIFY = FALSE)
    id <- olist$id
    olist[olist == ""] <- NULL
    if(olist$type == "stdout"){
        olist$id <- NULL
        olist$streamable <- NULL
        olist$outputBinding <- NULL
    }
    olist <- list(olist)
    names(olist) <- id
    return(olist)
}



.readInputs <- function(cwl.origin, cwl){
    inputList <- cwl.origin$inputs
    iList <- SimpleList()
    for(i in seq_along(inputList)){
        if(cwlClass(cwl) == "Workflow"){
            ilist <- c(id = names(inputList)[i], inputList[[i]])
        }else{
            ilist <- inputList[[i]]
        }
        if("inputBinding" %in% names(ilist)){
            idx <- match("inputBinding", names(ilist))
            ilist <- c(ilist[-idx], ilist[[idx]])
        }
        if(any(!names(ilist) %in% formalArgs(InputParam))){
            idx <- names(ilist) %in% formalArgs(InputParam)
            warning(names(ilist)[!idx], " not imported")
            ilist <- ilist[idx]
        }
        ilist$id <- names(inputList)[i]
        iList[[i]] <- do.call(InputParam, ilist)
    }
    names(iList) <- names(inputList)
    ## if(cwlClass(cwl) == "Workflow"){
    ##     names(iList) <- names(inputList)
    ## }else{
    ##     names(iList) <- lapply(inputList, "[[", "id")
    ## }
    return(iList)
}


.readOutputs <- function(cwl.origin, cwl){
    outputList <- cwl.origin$outputs
    oList <- SimpleList()
    if(length(outputList) == 0){
        return(oList)
    }
    for(i in seq_along(outputList)){
        if(cwlClass(cwl) == "Workflow"){
            olist <- c(id = names(outputList)[i], outputList[[i]])
        }else{
            olist <- outputList[[i]]
        }
        if("outputBinding" %in% names(olist)){
            idx <- match("outputBinding", names(olist))
            olist <- c(olist[-idx], olist[[idx]])
        }
        if(any(!names(olist) %in% formalArgs(OutputParam))){
            idx <- names(olist) %in% formalArgs(OutputParam)
            warning(names(olist)[!idx], " not imported")
            olist <- olist[idx]
        }
        olist$id <- names(outputList)[i]
        oList[[i]] <- do.call(OutputParam, olist)
    }
    names(oList) <- names(outputList)
    ## if(cwlClass(cwl) == "Workflow"){
    ##     names(oList) <- names(outputList)
    ## }else{
    ##     names(oList) <- lapply(outputList, "[[", "id")
    ## }
    return(oList)
}

.readSteps <- function(cwl.origin, pwd, cwl){
    Steps <- cwl.origin$steps
    for(i in seq_along(Steps)){
        id <- names(Steps)[i]
        sList <- c(id = id, Steps[[i]])
        run <- Steps[[i]]$run
        if(!grepl("^/", run)){
            sList$run <- file.path(pwd, run)
        }

        In <- sList$"in"
        slist <- list()
        for(i in seq(In)) {
            if(is.list(In[[i]])) {
                si <- stepInParam(id = names(In)[i])
                for(j in seq(In[[i]])){
                    slot(si, names(In[[i]])[j]) <- In[[i]][[j]]
                }
            }else{
                si <- stepInParam(id = names(In)[i],
                                  source = In[[i]])
            }
            slist[[i]] <- si
        }
        names(slist) <- names(In)
        sList$In <- stepInParamList(slist)
        sList$Out <- as.list(sList$out)
        sList$"in" <- NULL
        sList$"out" <- NULL
        cwl <- cwl + do.call(stepParam, sList)
    }
    return(cwl)
}

#' Read CWL
#' Function to read CWL command or workflow files.
#' @param cwlfile The cwl file to read.
#' @export
#' @return A object of class `cwlParam` or `cwlStepParam`.
#' @examples
#' input1 <- InputParam(id = "sth")
#' echo <- cwlParam(baseCommand = "echo",
#'                  inputs = InputParamList(input1))
#' tf <- tempfile()
#' writeCWL(echo, tf)
#' readCWL(paste0(tf, ".cwl"))
readCWL <- function(cwlfile){
    cwl.origin <- read_yaml(cwlfile)
    names(cwl.origin)[names(cwl.origin)=="class"] <- "cwlClass"
    cwl.list1 <- cwl.origin[setdiff(names(cwl.origin),
                                    c("inputs", "outputs", "steps"))]
    idx <- names(cwl.list1) %in% formalArgs(cwlParam)
    if(any(!idx)){
        warning(names(cwl.list1)[!idx], " not imported")
    }
    if(cwl.origin$cwlClass == "CommandLineTool"){
        if(!is.null(cwl.list1$arguments)){
            cwl.list1$arguments <- as.list(cwl.list1$arguments)
        }
        cwl <- do.call(cwlParam, cwl.list1[idx])
    }else{
        cwl <- do.call(cwlStepParam, cwl.list1[idx])
    }
    cwl@inputs@inputs <- .readInputs(cwl.origin, cwl)
    cwl@outputs@outputs <- .readOutputs(cwl.origin, cwl)

    if(cwl.origin$cwlClass == "Workflow"){
        pwd <- dirname(normalizePath(cwlfile))
        cwl <- .readSteps(cwl.origin, pwd, cwl)
    }
    return(cwl)
}


.readInputs <- function(cwl.origin, cwl){
    inputList <- cwl.origin$inputs
    iList <- list()
    for(i in seq_along(inputList)){
        if(cwlClass(cwl) == "Workflow"){
            if(is.character(inputList[[i]])){
                ilist <- list(id = names(inputList)[i], type = inputList[[i]])
            }else{
                ilist <- c(id = names(inputList)[i], inputList[[i]])
            }
        }else{
            ilist <- inputList[[i]]
        }
        if("inputBinding" %in% names(ilist)){
            idx <- match("inputBinding", names(ilist))
            ilist <- c(ilist[-idx], ilist[[idx]])
        }else{
            ilist$position = -1
        }
        if(is(ilist$type, "list") &&
           unlist(ilist$type)[["type"]] == "array"){

            if(any(!names(ilist) %in% formalArgs(InputParam))){
                idx <- names(ilist) %in% formalArgs(InputParam)
                warning(names(ilist)[!idx], " not imported")
                ilist <- ilist[idx]
            }
            idx <- names(ilist$type) %in% formalArgs(InputArrayParam)
            ilist$type <- do.call(InputArrayParam, ilist$type[idx])
        }
        if(is.null(ilist$id)){
            ilist$id <- names(inputList)[i]
        }
        iList[[i]] <- do.call(InputParam, ilist)
    }
    names(iList) <- names(inputList)
    return(iList)
}


.readOutputs <- function(cwl.origin, cwl){
    outputList <- cwl.origin$outputs
    oList <- list()
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
        if(is.null(olist$id)){
            olist$id <- names(outputList)[i]
        }
        oList[[i]] <- do.call(OutputParam, olist)
    }
    names(oList) <- names(outputList)
    return(oList)
}

.readSteps <- function(cwl.origin, cwl){
    Steps <- cwl.origin$steps
    for(i in seq_along(Steps)){
        if(is.null(Steps[[i]]$id)){
            id <- names(Steps)[i]
            sList <- c(id = id, Steps[[i]])
        }else{
            sList <- Steps[[i]]
        }
        run <- Steps[[i]]$run
        if(!grepl("^/", run)){
            sList$run <- run #file.path(pwd, run)
        }

        In <- sList$"in"
        slist <- list()
        for(i in seq(In)) {
            if(is.null(names(In)[i])){
                id <- In[[i]]$id
            }else{
                id <- names(In)[i]
            }
            if(is.list(In[[i]])) {
                si <- stepInParam(id = id)
                for(j in seq(In[[i]])){
                    slot(si, names(In[[i]])[j]) <- In[[i]][[j]]
                }
            }else{
                si <- stepInParam(id = id,
                                  source = In[[i]])
            }
            slist[[i]] <- si
        }
        names(slist) <- names(In)
        sList$In <- do.call(stepInParamList, slist)
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
#' @return A object of class `cwlProcess` or `cwlWorkflow`.
#' @examples
#' input1 <- InputParam(id = "sth")
#' echo <- cwlProcess(baseCommand = "echo",
#'                  inputs = InputParamList(input1))
#' tf <- tempfile()
#' writeCWL(echo, tf)
#' readCWL(paste0(tf, ".cwl"))
readCWL <- function(cwlfile){
    float.handler <- function(x){
        if(x == "."){
            return(".")
        }else{
            return(as.numeric(x))
        }
    }
    handlers <- list("float#fix" = float.handler)
    cwl.origin <- read_yaml(cwlfile, handlers = handlers)
    ## fix listing
    if(("InitialWorkDirRequirement" %in% names(cwl.origin$requirements)) &&
       is(cwl.origin$requirements$InitialWorkDirRequirement$listing, "character")){
        cwl.origin$requirements$InitialWorkDirRequirement$listing <-
            list(cwl.origin$requirements$InitialWorkDirRequirement$listing)
    }

    names(cwl.origin)[names(cwl.origin)=="class"] <- "cwlClass"
    cwl.list1 <- cwl.origin[setdiff(names(cwl.origin),
                                    c("inputs", "outputs", "steps"))]
    idx <- names(cwl.list1) %in% formalArgs(cwlProcess)
    if(cwl.origin$cwlClass %in% c("CommandLineTool", "ExpressionTool")){
        if(!is.null(cwl.list1$arguments)){
            cwl.list1$arguments <- as.list(cwl.list1$arguments)
        }
        cwl <- do.call(cwlProcess, cwl.list1[idx])
    }else if(cwl.origin$cwlClass == "Workflow"){
        cwl <- do.call(cwlWorkflow, cwl.list1[idx])
    }else {
        stop(cwl.origin$cwlClass, "is not supported!")
    }
    cwl@inputs@listData <- .readInputs(cwl.origin, cwl)
    cwl@outputs@listData <- .readOutputs(cwl.origin, cwl)

    if(cwl.origin$cwlClass == "Workflow"){
        ## pwd <- dirname(normalizePath(cwlfile))
        cwl <- .readSteps(cwl.origin, cwl)
    }
    
    if(any(!idx)){
        cwl@extensions <- cwl.list1[!idx]
    }
    return(cwl)
}

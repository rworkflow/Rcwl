#' cwlParam methods
#' @rdname cwlParam-methods
#' @export
#' @return cwlVersion: cwl version
cwlVersion <- function(cwl) cwl@cwlVersion

#' cwlVersion
#' CWL document version
#' @export
#' @param cwl A `cwlParam` object.
#' @param value Assign value to the `cwlParam` object.
#' @rdname cwlParam-methods
"cwlVersion<-" <- function(cwl, value){
    cwl@cwlVersion  <- value
    cwl
}

#' cwlClass
#' @rdname cwlParam-methods
#' @export
#' @return cwlClass: CWL Class
cwlClass <- function(cwl) cwl@cwlClass
#' cwlClass
#' @rdname cwlParam-methods
#' @export
"cwlClass<-" <- function(cwl, value){
    cwl@cwlClass <- value
    cwl
}

#' baseCommand
#' @rdname cwlParam-methods
#' @export
#' @return baseCommand: CWL baseCommand
baseCommand <- function(cwl) cwl@baseCommand
#' baseCommand
#' @rdname cwlParam-methods
#' @export
"baseCommand<-" <- function(cwl, value){
    cwl@baseCommand <- value
    cwl
}

#' arguments
#' @rdname cwlParam-methods
#' @export
#' @return arguments: CWL arguments
#' @param step To specifiy a step ID when `cwl` is a workflow. It can
#'     be multiple levels of steps separated by "/" for nested
#'     workflow.
arguments <- function(cwl, step = NULL){
    if(cwlClass(cwl) == "CommandLineTool"){
        cwl@arguments        
    }else if(cwlClass(cwl) == "Workflow"){
        ## allRun(cwl)[[step]]@arguments
        stopifnot(!is.null(step))
        Steps <- unlist(strsplit(step, split = "/"))
        irun <- "cwl@steps@steps[[ Steps[1] ]]@run"
        if(length(Steps) > 1){
            for(i in 2:length(Steps)){
                irun <- paste0(irun, "@steps@steps[[ Steps[", i, "] ]]@run")
            }
        }
        irun <- paste0(irun, "@arguments")
        eval(parse(text = irun))
    }
}
#' arguments
#' @rdname cwlParam-methods
#' @export
"arguments<-" <- function(cwl, step = NULL, value){
    if(cwlClass(cwl) == "CommandLineTool"){
        cwl@arguments <- value
    }else if(cwlClass(cwl) == "Workflow"){
        stopifnot(!is.null(step))
        Steps <- unlist(strsplit(step, split = "/"))
        irun <- "cwl@steps@steps[[ Steps[1] ]]@run"
        if(length(Steps) > 1){
            for(i in 2:length(Steps)){
                irun <- paste0(irun, "@steps@steps[[ Steps[", i, "] ]]@run")
            }
        }
        value <- paste0(unlist(value), collapse = "\", \"")
        irun <- paste0(irun, "@arguments <- list(\"", value, "\")")
        eval(parse(text = irun))
    }
    cwl
}

#' hints
#' @export
#' @rdname cwlParam-methods
#' @return hints: CWL hints
hints <- function(cwl) cwl@hints
#' hints
#' @rdname cwlParam-methods
#' @export
"hints<-" <- function(cwl, value){
    cwl@hints <- value
    cwl
}

#' requirements
#' @rdname cwlParam-methods
#' @export
#' @return requirements: CWL requirments
requirements <- function(cwl) cwl@requirements
#' requirements
#' @rdname cwlParam-methods
#' @export
"requirements<-" <- function(cwl, value){
    cwl@requirements <- value
    cwl
}

#' inputs
#' @rdname InputParamList
#' @param cwl A cwlParam object
#' @export
#' @return inputs: A list of `InputParam`.
#' @examples
#' ## Inputs
#' input1 <- InputParam(id = "sth")
#' echo <- cwlParam(baseCommand = "echo", inputs = InputParamList(input1))
#' inputs(echo)
inputs <- function(cwl) cwl@inputs

.assignInput <- function(x, name, value){
    stopifnot(name %in% names(inputs(x)))
    itype <- inputs(x)[[name]]@type
    if(is(itype, "InputArrayParam")){
        if(inputs(x)[[name]]@type@items == "File"){
            v <- lapply(value, function(x)list(class="File",
                                               path=normalizePath(x)))
        }else{
            v <- value
        }
    }else if(is(itype, "character")){
        ## optional type
        if(grepl("?", itype)){
            itype <- sub("\\?", "", itype)
        }
        if(itype == "int"){
            v <- as.integer(value)
        }else if(itype %in% c("File", "Directory")){
            if(!file.exists(value)) stop(value, " does not exist!")
            v <- list(class = itype, path = normalizePath(value))
        }else if(itype == "File[]"){
            v <- lapply(value, function(x)list(class="File",
                                               path=normalizePath(x)))
        }else{
            v <- value
        }
    }
    x@inputs[[name]]@value <- v
    x
}

#' @importFrom utils .DollarNames
#' @export
.DollarNames.cwlParam <- function(x, pattern = "") {
    grep(pattern, names(inputs(x)), value = TRUE)
}

#' Extract input values by name
#' @rdname InputParam
#' @importFrom S4Vectors wmsg
#' @export
setMethod("$", "cwlParam", function(x, name){
    if(name %in% names(inputs(x))){
        inputs(x)[[name]]@value
    }else{
        stop(wmsg("the '", name, "' does not exist"))
    }
})

#' Set input values by name
#' @param x A `cwlParam` object.
#' @param name One one of input list
#' @export
#' @rdname InputParam
setReplaceMethod("$", "cwlParam", function(x, name, value){
    .assignInput(x, name, value)
})

setGeneric("$")

#' outputs
#' The outputs of a cwlParam object
#' @param cwl A cwlParam object
#' @rdname OutputParamList
#' @export
#' @return outputs: A list of `OutputParam`.
#' @examples
#' input1 <- InputParam(id = "sth")
#' echo <- cwlParam(baseCommand = "echo", inputs = InputParamList(input1))
#' outputs(echo) 
outputs <- function(cwl) {
    if(is(cwl@outputs, "list")) {
        cwl@outputs
    }else{
        cwl@outputs@outputs
    }
}

#' stdout of cwlParam
#' @rdname cwlParam-methods
#' @export
#' @return stdOut: CWL stdout
stdOut <- function(cwl) cwl@stdout
#' stdout of cwlParam
#' @rdname cwlParam-methods
#' @export
"stdOut<-" <- function(cwl, value){
    cwl@stdout <- value
    cwl
}

setMethod(show, "InputParamList", function(object) {
    cat("inputs:\n")
    lapply(seq(object), function(i){
        if(object[[i]]@label != ""){
            iname <- paste0(names(object)[i], " (", object[[i]]@label, ") ")
        }else{
            iname <- names(object)[i]
        }
        if(is(object[[i]]@type, "character")){
            if(length(object[[i]]@value) > 0){
                v <- object[[i]]@value
            }else{
                v <- object[[i]]@default
            }
            if(object[[i]]@type %in% c("File", "Directory")){
                if(length(object[[i]]@value) > 0){
                    v <- object[[i]]@value$path
                }else{
                    v <- object[[i]]@value
                }
            }

            cat("  ", iname, " (", object[[i]]@type, "): ",
                object[[i]]@inputBinding$prefix, " ",
                paste(unlist(v), collapse = " "), "\n", sep = "")
        }else if(is(object[[i]]@type, "InputArrayParam")){
            if(object[[i]]@type@items %in% c("File", "Directory")){
                if(length(object[[i]]@value) > 0){
                    v <- object[[i]]@value$path
                }else{
                    v <- object[[i]]@value
                }
            }else{
                v <- object[[i]]@value
            }
            cat("  ", iname, ":\n", sep = "")
            cat("    type: ", object[[i]]@type@type, "\n", sep = "")
            cat("    prefix: ",
                object[[i]]@type@inputBinding$prefix, " ",
                paste(unlist(v), collapse=" "), "\n", sep = "")
        }
    })
})

setMethod(show, "OutputParamList", function(object) {
    cat("outputs:\n")
    cat(as.yaml(as.listOutputs(object@outputs)))
    ## lapply(seq(object@outputs), function(j){
    ##     cat("  ", object@outputs[[j]]@id, ":\n", sep = "")
        
    ##     if(is(object@outputs[[j]]@type, "OutputArrayParam")){
    ##         cat("    type: array\n")
    ##     }else{
    ##         cat("    type: ", object@outputs[[j]]@type, "\n", sep = "")
    ##         if(object@outputs[[j]]@type != "stdout"){
    ##             if(length(object@outputs[[j]]@outputBinding$glob) > 0){
    ##                 cat("      glob: ", object@outputs[[j]]@outputBinding$glob, "\n", sep = "")
    ##             }
    ##             if(length(object@outputs[[j]]@secondaryFiles) > 0){
    ##                 cat("      secondaryFiles: ", object@outputs[[j]]@secondaryFiles, "\n", sep = "")
    ##             }
    ##             if(length(object@outputs[[j]]@outputSource) > 0){
    ##                 cat("    outputSource: ", object@outputs[[j]]@outputSource, "\n", sep = "")
    ##             }
    ##         }
    ##     }
    ## })
})

setMethod(show, "cwlParam", function(object){
    cat("class:", class(object), "\n",
        "cwlClass:", cwlClass(object), "\n",
        "cwlVersion:", cwlVersion(object), "\n",
        "baseCommand:", baseCommand(object), "\n")
    if(length(object@requirements) > 0){
        cat("requirements:\n")
        cat(as.yaml(object@requirements))
    }
    if(length(object@hints) > 0){
        cat("hints:\n")
        cat(as.yaml(object@hints))
    }
    if(length(object@arguments) > 0){
        cat("arguments:", unlist(object@arguments), "\n")
    }
    show(object@inputs)
    show(object@outputs)
    if(length(object@stdout) > 0) cat("stdout:", object@stdout, "\n")
})

setMethod(show, "cwlStepParam", function(object){
    cat("class:", class(object), "\n",
        "cwlClass:", cwlClass(object), "\n",
        "cwlVersion:", cwlVersion(object), "\n")
    if(length(object@requirements) > 0){
        cat("requirements:\n")
        cat(as.yaml(object@requirements))
    }
    if(length(object@hints) > 0){
        cat("hints:\n")
        cat(as.yaml(object@hints))
    }
    if(length(object@arguments) > 0){
        cat("arguments:", unlist(object@arguments), "\n")
    }
    show(object@inputs)
    show(object@outputs)
    if(length(object@stdout) > 0) cat("stdout:", object@stdout, "\n")
    if(cwlClass(object) == "Workflow") {
        show(object@steps)
    }
})

#' short
#' 
#' The function to show short summary of cwlParam or cwlStepParam
#' @param object An cwlParam or cwlStepParam object
#' @export
#' @return A short summary of an object of cwlParam or cwlStepParam.
#' @examples
#' s1 <- cwlStepParam()
#' short(s1)
short <- function(object){
    if(is(object, "cwlParam")){
        cat(as.yaml(list(inputs = names(inputs(object)))))
        cat(as.yaml(list(outputs = names(outputs(object)))))
    }
    if(is(object, "cwlStepParam")) {
        cat(as.yaml(list(steps = names(steps(object)))))
    }
}

#' runs
#'
#' The function to access all runs of a cwlStepParam object
#' @param object A cwlStepParam object.
#' @export
#' @return cwlParam objects or paths of CWL file.
#' @examples
#' s1 <- cwlStepParam()
#' runs(s1)
runs <- function(object){
    stopifnot(is(object, "cwlStepParam"))
    SimpleList(lapply(steps(object), function(x)x@run))
}

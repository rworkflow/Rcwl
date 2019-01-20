
#' cwlParam methods
#' @rdname cwlParam-methods
#' @export
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
arguments <- function(cwl) cwl@arguments
#' arguments
#' @rdname cwlParam-methods
#' @export
"arguments<-" <- function(cwl, value){
    cwl@arguments <- value
    cwl
}

#' hints
#' @export
#' @rdname cwlParam-methods
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
inputs <- function(cwl) cwl@inputs@inputs

.assignInput <- function(x, name, value){
    stopifnot(name %in% names(inputs(x)))
    itype <- inputs(x)[[name]]@type
    if(is(itype, "InputArrayParam")){
        v <- value
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
            ##v <- unlist(v, recursive = FALSE)
        }else{
            v <- value
        }
    }
    x@inputs@inputs[[name]]@value <- v
    x
}

#' Extract input values by name
#' @rdname InputParam
#' @importFrom S4Vectors wmsg
#' @export
setMethod("$", "cwlParam", function(x, name){
    ## check if exist
    ins <- names(inputs(x))
    ins <- ins[startsWith(ins, name)]
    pattern <- sprintf("(%s*)/.*$", name)
    
    if(name %in% sub(pattern, "\\1", ins)){
        inputs(x)[[name]]@value
    }else{
        stop(wmsg("the '", name, "' does not exist"))
    }
    ##inputs(x)[[name]]@value
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
stdOut <- function(cwl) cwl@stdout
#' stdout of cwlParam
#' @rdname cwlParam-methods
#' @export
"stdOut<-" <- function(cwl, value){
    cwl@stdout <- value
    cwl
}

## #' Assign values to input params
## assignOutputGlob <- function(cwl, name="output", value){
##     itype <- cwl@outputs$output@type
##     stopifnot(itype != "File")
##     cwl@outputs$output@outputBinding$glob <- value
##     cwl
## }

setMethod(show, "InputParamList", function(object) {
    cat("inputs:\n")
    lapply(seq(object@inputs), function(i){
        if(object@inputs[[i]]@label != ""){
            iname <- paste0(names(object@inputs)[i], " (", object@inputs[[i]]@label, ") ")
        }else{
            iname <- names(object@inputs)[i]
        }
        if(is(object@inputs[[i]]@type, "character")){
            if(length(object@inputs[[i]]@value) > 0){
                v <- object@inputs[[i]]@value
            }else{
                v <- object@inputs[[i]]@default
            }
            if(object@inputs[[i]]@type %in% c("File", "Directory")){
                if(length(object@inputs[[i]]@value) > 0){
                    v <- object@inputs[[i]]@value$path
                }else{
                    v <- object@inputs[[i]]@value
                }
            }
            ## }else{
            ##     v <- object@inputs[[i]]@value
            ## }

            cat("  ", iname, " (", object@inputs[[i]]@type, "): ",
                object@inputs[[i]]@inputBinding$prefix, " ",
                paste(unlist(v), collapse = " "), "\n", sep = "")
        }else if(is(object@inputs[[i]]@type, "InputArrayParam")){
            if(object@inputs[[i]]@type@items %in% c("File", "Directory")){
                if(length(object@inputs[[i]]@value) > 0){
                    v <- object@inputs[[i]]@value$path
                }else{
                    v <- object@inputs[[i]]@value
                }
            }else{
                v <- object@inputs[[i]]@value
            }
            cat("  ", iname, ":\n", sep = "")
            cat("    type: ", object@inputs[[i]]@type@type, "\n", sep = "")
            cat("    prefix: ",
                object@inputs[[i]]@type@inputBinding$prefix, " ",
                paste(unlist(v), collapse=" "), "\n", sep = "")
        }
    })
})

setMethod(show, "OutputParamList", function(object) {
    cat("outputs:\n")
    lapply(seq(object@outputs), function(j){
        cat("  ", object@outputs[[j]]@id, ":\n", sep = "")
        
        if(is(object@outputs[[j]]@type, "OutputArrayParam")){
            cat("    type: array\n")
        }else{
            cat("    type: ", object@outputs[[j]]@type, "\n", sep = "")
            if(object@outputs[[j]]@type != "stdout"){
                if(length(object@outputs[[j]]@outputBinding$glob) > 0){
                    cat("      glob: ", object@outputs[[j]]@outputBinding$glob, "\n", sep = "")
                }
                if(length(object@outputs[[j]]@secondaryFiles) > 0){
                    cat("      secondaryFiles: ", object@outputs[[j]]@secondaryFiles, "\n", sep = "")
                }
                if(length(object@outputs[[j]]@outputSource) > 0){
                    cat("    outputSource: ", object@outputs[[j]]@outputSource, "\n", sep = "")
                }
            }
        }
    })
})

setMethod(show, "cwlParam", function(object){
    cat("class:", class(object), "\n")
    cat("cwlClass:", cwlClass(object), "\n")
    cat("cwlVersion:", cwlVersion(object), "\n")
    cat("baseCommand:", baseCommand(object), "\n")
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
    ## if(cwlClass(object) == "Workflow") {
    ##     show(object@steps)
    ## }
})

setMethod(show, "cwlStepParam", function(object){
    cat("class:", class(object), "\n")
    cat("cwlClass:", cwlClass(object), "\n")
    cat("cwlVersion:", cwlVersion(object), "\n")
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
#' @rdname cwlParam-methods
#' @export
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
#' @rdname cwlParam-methods
#' @export
runs <- function(object){
    stopifnot(is(object, "cwlStepParam"))
    SimpleList(lapply(steps(object), function(x)x@run))
}

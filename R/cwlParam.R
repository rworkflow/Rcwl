
#' cwlVersion
#' @rdname cwlClass
#' @export
cwlVersion <- function(cwl) cwl@cwlVersion

#' cwlVersion
#' CWL document version
#' @export
#' @param cwl A cwlParam object.
#' @param value CWL document version.
#' @rdname cwlVersion
"cwlVersion<-" <- function(cwl, value){
    cwl@cwlVersion  <- value
    cwl
}

#' cwlClass
#' @rdname cwlClass
#' @export
cwlClass <- function(cwl) cwl@cwlClass
#' cwlClass
#' @rdname cwlClass
#' @param cwl A cwlParam object
#' @param value The cwlParam class.
#' @export
"cwlClass<-" <- function(cwl, value){
    cwl@cwlClass <- value
    cwl
}

#' baseCommand
#' @rdname cwlClass
#' @export
baseCommand <- function(cwl) cwl@baseCommand
#' baseCommand
#' @rdname baseCommand
#' @param cwl A cwlParam object
#' @param value The class of CWL
#' @export
"baseCommand<-" <- function(cwl, value){
    cwl@baseCommand <- value
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
        if(itype == "int"){
            v <- as.integer(value)
        }else if(itype %in% c("File", "Directory")){
            if(!file.exists(value)) stop(value, " does not exist!")
            v <- list(class = itype, path = normalizePath(value))
        }else{
            v <- value
        }
    }
    x@inputs@inputs[[name]]@value <- v
    x
}

#' Extract input values by name
#' @rdname InputParam
#' @export
setMethod("$", "cwlParam", function(x, name){
    inputs(x)[[name]]@value
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

            cat("  ", iname, ": ",
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
                if(length(object@outputs[[j]]@outputSource) > 0){
                    cat("    outputSource: ", object@outputs[[j]]@outputSource, "\n", sep = "")
                }
            }
        }
    })
})

setMethod(show, "cwlParam", function(object){
    cat("class: cwlParam\n")
    cat("cwlClass:", cwlClass(object), "\n")
    cat("cwlVersion:", cwlVersion(object), "\n")
    cat("baseCommand:", baseCommand(object), "\n")
    if(length(object@requirements) > 0){
        cat("requirements:\n",
            names(unlist(object@requirements)), ":", unlist(object@requirements), "\n")
    }
    if(length(object@hints) > 0){
        cat("hints:\n",
            names(unlist(object@hints)), ":", unlist(object@hints), "\n")
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

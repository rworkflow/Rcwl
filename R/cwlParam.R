#' Parameters for cwl
#'
setClass("cwlParam",
         slots = c(
             cwlVersion = "character",
             cwlClass = "character",
             baseCommand = "character",
             inputs = "InputParamList",
             outputs = "OutputParam"
         ),
         prototype = list(cwlVersion = character(),
                          cwlClass = character(),
                          baseCommand = character(),
                          inputs = InputParamList(),
                          outputs = OutputParam()
         ))

cwlParam <- function(cwlVersion = "v1.0", cwlClass = "CommandLineTool",
                     baseCommand = "", inputs = InputParamList(),
                     outputs = OutputParam(), paramList = list()){
    new("cwlParam", cwlVersion = cwlVersion, cwlClass = cwlClass,
        baseCommand = baseCommand, inputs = inputs, outputs = outputs)
}

#' cwlVersion
cwlVersion <- function(cwl) cwl@cwlVersion

## "cwlVersion<-" <- function(cwl, value){cwl}
setGeneric("cwlVersion<-")
setReplaceMethod("cwlVersion", "cwlParam", function(object, value){
    object@cwlVersion <- value
    object
})

#' cwlClass
cwlClass <- function(cwl) cwl@cwlClass
## "cwlClass<-" <- function(cwl, value){cwl}
setGeneric("cwlClass<-")
setReplaceMethod("cwlClass", "cwlParam", function(object, value){
    object@cwlClass <- value
    object
})

#' baseCommand
baseCommand <- function(cwl) cwl@baseCommand
## "baseCommand<-" <- function(cwl, value){cwl}
setGeneric("baseCommand<-")
setReplaceMethod("baseCommand", "cwlParam", function(object, value){
    object@baseCommand <- value
    object
})

#' inputs
inputs <- function(cwl) cwl@inputs@inputs

#' Assign values to input params
## #' @param names list
## #' @param values list
## assignInputs <- function(cwl, names, values){
##     for(i in 1:length(names)){
##         itype <- inputs(cwl)[[names[[i]]]]@type
##         if(itype == "int"){
##             v <- as.integer(values[[i]])
##         }else if(itype == "File"){
##             v <- list(class = "File", path = normalizePath(values[[i]]))
##         }else{
##             v <- values[[i]]
##         }
##         cwl@inputs@inputs[[names[[i]]]]@value <- v
##     }
##     cwl
## }

#' assign value to each input list
.assignInput <- function(x, name, value){
    itype <- inputs(x)[[name]]@type
    if(itype == "int"){
        v <- as.integer(value)
    }else if(itype == "File"){
        if(!file.exists(value)) stop(value, " does not exist!")
        v <- list(class = "File", path = normalizePath(value))
    }else{
        v <- value
    }
    x@inputs@inputs[[name]]@value <- v
    x
}

#' @export
setMethod("$", "cwlParam", function(x, name){
    inputs(x)[[name]]@value
})

#' @export
setReplaceMethod("$", "cwlParam", function(x, name, value){
    .assignInput(x, name, value)
})


#' Input parameters
setClass("InputParam",
         slots = c(
             id = "character",
             label = "character",
             type = "character",
             inputBinding = "list",
             value = "ANY"
         ),
         prototype = list(label = character(),
                          type = character(),
                          inputBinding = list(position = integer(),
                                              prefix = character(),
                                              separate = logical()),
                          value = character())
         )

InputParam <- function(id, label= "", type = "string", position = 0, prefix = "", separate = TRUE, value = vector()){
    new("InputParam",
        id = id,
        label = label,
        type = type,
        inputBinding = list(position = position, prefix = prefix, separate = separate),
        value = value)
}

setClass("InputParamList", slots = c(inputs = "SimpleList"))

InputParamList <- function(...){
    iList <- SimpleList(...)
    names(iList) <- lapply(iList, function(x)x@id)
    new("InputParamList", inputs = iList)
}

#' Output parameters
setClass("OutputParam",
         slots = c(
             id = "character",
             label = "character",
             type = "character",
             items = "character",
             streamable = "character",
             outputBinding = "list"
         ),
         prototype = list(label = character(),
                          type = character(),
                          outputBinding = list(glob = character()))
         )

OutputParam <- function(id = "output", label = "", type = "stdout", items = "", streamable = "false", glob = ""){
    if(items %in% c("File", "Directory") && type != "array") stop("type must be array!")
    new("OutputParam",
        id = id,
        label = label,
        type = type,
        streamable = streamable,
        items = items,
        outputBinding = list(glob = glob))
}

## setClass("OutputParamList", slots = c(outputs = "SimpleList"))
## OutputParamList <- function(out = OutputParam(), ...){
##     oList <- SimpleList(out = out, ...)
##     if(length(oList) > 0 & is.null(names(oList))){
##         stop("Outputs must be named!")
##     }
##     new("OutputParamList", outputs = oList)
## }

#' outputs
outputs <- function(cwl) cwl@outputs

#' Assign values to input params
assignOutputGlob <- function(cwl, name="output", value){
    itype <- cwl@outputs$output@type
    stopifnot(itype != "File")
    cwl@outputs$output@outputBinding$glob <- value
    cwl
}

#' show method
setMethod(show, "cwlParam", function(object){
    cat("class: cwlParam\n")
    cat("cwlVersion:", cwlVersion(object), "\n")
    cat("baseCommand:", baseCommand(object), "\n")
    cat("inputs:\n")
    lapply(seq(inputs(object)), function(i){
        if(inputs(object)[[i]]@label != ""){
            iname <- paste0(names(inputs(object))[i], " (", inputs(object)[[i]]@label, ") ")
        }else{
            iname <- names(inputs(object))[i]
        }
        if(inputs(object)[[i]]@type == "File"){
            if(length(inputs(object)[[i]]@value) > 0){
                v <- inputs(object)[[i]]@value$path
            }else{
                v <- inputs(object)[[i]]@value
            }
        }else{
            v <- inputs(object)[[i]]@value
        }
        cat("  ", iname, ": ",
            inputs(object)[[i]]@inputBinding$prefix, " ",
            v, "\n", sep = "")
    })
    cat("outputs:\n")
    cat("  ", object@outputs@id, ":\n", sep = "")
    cat("    type: ", object@outputs@type, "\n", sep = "")
    if(object@outputs@type != "stdout"){
        cat("      glob: ", object@outputs@outputBinding$glob, "\n", sep = "")
    }
})



#' InputArrayParam
#' @param items Defines the type of the array elements when type is a InputArrayParam class.
setClass("InputArrayParam",
         slots = c(
             label = "character",
             type = "character",
             items = "character",
             inputBinding = "list"
         ),
         prototype = list(
             label = character(),
             type = "string",
             items = character(),
             inputBinding = list(prefix = character(),
                                 separate = logical(),
                                 itemSeparator = character(),
                                 valueFrom = character()))
         )
#' @export
InputArrayParam <- function(label= "", type = "string", items = character(), prefix = "", separate = TRUE, itemSeparator = character(), valueFrom = character()){
    new("InputArrayParam",
        label = label,
        type = type,
        items = items,
        inputBinding = list(prefix = prefix,
                            separate = separate,
                            itemSeparator = itemSeparator,
                            valueFrom = valueFrom))
}

#' Input parameters
setClassUnion("characterORInputArrayParam", c("character", "InputArrayParam"))

setClass("InputParam",
         slots = c(
             id = "character",
             label = "character",
             type = "characterORInputArrayParam",
             inputBinding = "list",
             default = "ANY",
             value = "ANY"
         ),
         prototype = list(
             id = character(),
             label = character(),
             type = character(),
             inputBinding = list(position = integer(),
                                 prefix = character(),
                                 separate = logical(),
                                 itemSeparator = character(),
                                 valueFrom = character()),
             default = character(),
             value = character())
         )
#' @export
InputParam <- function(id, label= "", type = "string", position = 0L, prefix = "", separate = TRUE, itemSeparator = character(), valueFrom = character(), default = character(), value = character()){
    new("InputParam",
        id = id,
        label = label,
        type = type,
        inputBinding = list(position = as.integer(position),
                            prefix = prefix,
                            separate = separate,
                            itemSeparator = itemSeparator,
                            valueFrom = valueFrom),
        default = default,
        value = value)
}

#' InputParamList
setClass("InputParamList", representation(inputs = "SimpleList"),
         prototype = list(elementType = "InputParam"),
         contains = "SimpleList")

#' @export
InputParamList <- function(...){
    iList <- SimpleList(...)
    names(iList) <- lapply(iList, function(x)x@id)
    new("InputParamList", inputs = iList)
    ##S4Vectors:::new_SimpleList_from_list("InputParamList", iList)
}

#' Output array parameters
setClass("OutputArrayParam",
         slots = c(
             label = "character",
             type = "character",
             items = "character",
             outputBinding = "list"
         ),
         prototype = list(
             label = character(),
             type = "string",
             items = character(),
             outputBinding = list(glob = character(),
                                 loadContents = logical(),
                                 outputEval = character()))
         )
#' @export
OutputArrayParam <- function(label= character(), type = "string", items = character(),
                             glob = character(), loadContents = logical(),
                             outputEval = character()){
    new("OutputArrayParam",
        label = label,
        type = type,
        items = items,
        outputBinding = list(glob = glob,
                            localContents = loadContents,
                            outputEval = outputEval))
}

setClassUnion("characterOROutputArrayParam", c("character", "OutputArrayParam"))

#' Output parameters
#' @param items from step parameters
setClass("OutputParam",
         slots = c(
             id = "character",
             label = "character",
             type = "characterOROutputArrayParam",
             secondaryFiles = "character",
             streamable = "logical",
             outputBinding = "list",
             items = "character",
             outputSource = "character"
         ),
         prototype = list(id = character(),
                          label = character(),
                          type = character(),
                          secondaryFiles = character(),
                          streamable = logical(),
                          items = character(),
                          outputBinding = list(glob = character(),
                                               loadContents = logical(),
                                               outputEval = character()),
                          outputSource = character())
         )

#' @export
OutputParam <- function(id = "output", label = character(), type = "stdout",
                        secondaryFiles = character(), items = character(), streamable = logical(),
                        glob = character(), loadContents = logical(), outputEval = character(),
                        outputSource = character()){
    ##if(items %in% c("File", "Directory") && type != "array") stop("type must be array!")
    new("OutputParam",
        id = id,
        label = label,
        type = type,
        secondaryFiles = secondaryFiles,
        streamable = streamable,
        items = items,
        outputBinding = list(glob = glob,
                             loadContents = loadContents,
                             outputEval = outputEval),
        outputSource = outputSource)
}

#' OutputParamList
setClass("OutputParamList", representation(outputs = "SimpleList"),
         prototype = list(elementType = "OutputParam"),
         contains = "SimpleList")

#' @export
OutputParamList <- function(out = OutputParam(), ...){
    oList <- SimpleList(out, ...)
    names(oList) <- lapply(oList, function(x)x@id)
    new("OutputParamList", outputs = oList)
}

## setClass("OutputParamList", slots = c(outputs = "SimpleList"))
## #' @export
## OutputParamList <- function(out = OutputParam(), ...){
##     oList <- SimpleList(out, ...)
##     names(oList) <- lapply(oList, function(x)x@id)
##     new("OutputParamList", outputs = oList)
## }

#' Parameters for cwl
#' @importFrom S4Vectors SimpleList
#' @param hints Any or a list.
setClassUnion("OutputParamListORlist", c("OutputParamList", "list"))

setClass("cwlParam",
         slots = c(
             cwlVersion = "character",
             cwlClass = "character",
             baseCommand = "character",
             requirements = "list",
             hints = "list",
             arguments = "list",
             inputs = "InputParamList",
             outputs = "OutputParamListORlist",
             stdout = "character"
         ),
         prototype = list(cwlVersion = character(),
                          cwlClass = character(),
                          baseCommand = character(),
                          requirements = list(),
                          hints = list(),
                          arguments = list(),
                          inputs = InputParamList(),
                          outputs = OutputParamList(),
                          stdout = character()
         ))
#' @export
cwlParam <- function(cwlVersion = "v1.0", cwlClass = "CommandLineTool",
                     baseCommand = character(), requirements = list(),
                     hints = list(), arguments = list(),
                     inputs = InputParamList(), outputs = OutputParamList(),
                     stdout = character()){
    new("cwlParam", cwlVersion = cwlVersion, cwlClass = cwlClass,
        baseCommand = baseCommand, requirements = requirements, hints = hints,
        arguments = arguments, inputs = inputs, outputs = outputs, stdout = stdout)
}

#' stepInParam
setClass("stepInParam",
         slots = c(id = "character",
                   source = "character",
                   linkMerge = "character",
                   default = "ANY",
                   valueFrom = "character"))
stepInParam <- function(id, source = character(), linkMerge = character(), default = character(), valueFrom = character()){
    new("stepInParam",
        id = id, source = source, linkMerge = linkMerge,
        default = default, valueFrom = valueFrom)
}

#' stepInParamList
setClass("stepInParamList", representation(Ins = "SimpleList"),
         prototype = list(elementType = "stepInParam"),
         contains = "SimpleList")

#' @export
stepInParamList <- function(...){
    iList <- SimpleList(...)
    names(iList) <- lapply(iList, function(x)x@id)
    new("stepInParamList", Ins = iList)
}

## setClass("stepInParamList", slots = c(Ins = "SimpleList"), contains = "SimpleList")
## stepInParamList <- function(...){
##     iList <- SimpleList(...)
##     names(iList) <- lapply(iList, function(x)x@id)
##     new("stepInParamList", Ins = iList)
## }

#' stepParam
#' @export
setClass("stepParam",
         slots = c(id = "character",
                   run = "cwlParam",
                   In = "stepInParamList",
                   Out = "list"))
stepParam <- function(id, run = cwlParam(), In = stepInParamList(), Out = list()) {
    new("stepParam",
        id = id,
        run = run,
        In = In,
        Out = Out)
}

#' stepParamList
setClass("stepParamList", representation(steps = "SimpleList"),
         prototype = list(elementType = "stepParam"),
         contains = "SimpleList")

#' @export
stepParamList <- function(...){
    iList <- SimpleList(...)
    names(iList) <- lapply(iList, function(x)x@id)
    new("stepParamList", steps = iList)
}

## setClass("stepParamList", slots = c(steps = "SimpleList"))
## stepParamList <- function(...){
##     iList <- SimpleList(...)
##     names(iList) <- lapply(iList, function(x)x@id)
##     new("stepParamList", steps = iList)
## }


#' cwlStepParam
#' @export
setClass("cwlStepParam",
         contains = "cwlParam",
         slots = c(steps = "stepParamList"),
         prototype = list(steps = stepParamList())
         )

cwlStepParam <- function(cwlVersion = "v1.0", cwlClass = "Workflow",
                     baseCommand = character(), requirements = list(),
                     hints = list(), arguments = list(),
                     inputs = InputParamList(), outputs = OutputParamList(),
                     stdout = character(), steps = stepParamList()){
    new("cwlStepParam", cwlVersion = cwlVersion, cwlClass = cwlClass,
        baseCommand = baseCommand, requirements = requirements, hints = hints,
        arguments = arguments, inputs = inputs, outputs = outputs, stdout = stdout,
        steps = steps)
}


#' InputArrayParam
#' @rdname InputArrayParam
#' @export
setClass("InputArrayParam",
         slots = c(
             label = "character",
             type = "character",
             items = "character",
             inputBinding = "list"
         ),
         prototype = list(
             label = character(),
             type = character(),
             items = character(),
             inputBinding = list(prefix = character(),
                                 separate = logical(),
                                 itemSeparator = character(),
                                 valueFrom = character()))
         )
#' InputArrayParam
#'
#' Parameters for array inputs. To specify an array parameter, the array definition is nested under the type field with 'type: array' and items defining the valid data types that may appear in the array. More details: https://www.commonwl.org/v1.0/CommandLineTool.html#CommandInputArraySchema
#'
#' @export
#' @rdname InputArrayParam
#' @param label A short description for this object
#' @param type Must be "array".
#' @param item Defines the type of the array elements.
#' @param prefix Command line prefix to add before the value.
#' @param separate If true (default), then the prefix and value must be added as separate command line arguments; if false, prefix and value must be concatenated into a single command line argument.
#' @param itemSeparator Join the array elements into a single string with separator.
#' @param valueFrom String or Expression.
InputArrayParam <- function(label = "", type = "array", items = character(), prefix = "", separate = TRUE, itemSeparator = character(), valueFrom = character()){
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

#' InputParam
#' @rdname InputParam
#' @export
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

#' InputParam
#' parameter for a command tool. More details: https://www.commonwl.org/v1.0/CommandLineTool.html#CommandInputParameter
#' @rdname InputParam
#' @param id The unique identifier for this parameter object.
#' @param label A short, human-readable label of this object.
#' @param type valid types of data that may be assigned to this parameter.
#' @param position The position for this parameter.
#' @param prefix Command line prefix to add before the value.
#' @param separate If true (default), then the prefix and value must be added as separate command line arguments; if false, prefix and value must be concatenated into a single command line argument.
#' @param itemSeparator Join the array elements into a single string with the elements separated by by itemSeparator.
#' @param valueFrom String or Expression.
#' @param default The default value for this parameter
#' @param value Assigned value for this parameter
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
#' @rdname InputParamList
#' @export
setClass("InputParamList", representation(inputs = "SimpleList"),
         prototype = list(elementType = "InputParam"),
         contains = "SimpleList")

#' InputParamList
#' A list of InputParam
#' @param ... The InputParam objects.
#' @rdname InputParamList
#' @export
InputParamList <- function(...){
    iList <- SimpleList(...)
    names(iList) <- lapply(iList, function(x)x@id)
    new("InputParamList", inputs = iList)
}


#' Output array parameters
#' @rdname OutputArrayParam
#' @export
setClass("OutputArrayParam",
         slots = c(
             label = "character",
             type = "character",
             items = "character",
             outputBinding = "list"
         ),
         prototype = list(
             label = character(),
             type = character(),
             items = character(),
             outputBinding = list(glob = character(),
                                 loadContents = logical(),
                                 outputEval = character()))
         )
#' Output array parameters
#' Parameters for array outputs. More details: https://www.commonwl.org/v1.0/CommandLineTool.html#CommandOutputArraySchema
#' @rdname OutputArrayParam
#' @param label A short, human-readable label of this object.
#' @param type Must be "array".
#' @param item Defines the type of the array elements.
#' @param glob Pattern to find files relative to the output directory.
#' @param loadContents Read text from globbed file.
#' @param outputEval Evaluate an expression to generate the output value.
#' @export
OutputArrayParam <- function(label= character(), type = "array", items = character(),
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
#' @rdname OutputParam
#' @export
setClass("OutputParam",
         slots = c(
             id = "character",
             label = "character",
             type = "characterOROutputArrayParam",
             secondaryFiles = "character",
             streamable = "logical",
             outputBinding = "list",
             outputSource = "character"
         ),
         prototype = list(id = character(),
                          label = character(),
                          type = character(),
                          secondaryFiles = character(),
                          streamable = logical(),
                          outputBinding = list(glob = character(),
                                               loadContents = logical(),
                                               outputEval = character()),
                          outputSource = character())
         )

#' Output parameters
#' An output parameter for a Command Line Tool. More details: https://www.commonwl.org/v1.0/CommandLineTool.html#CommandOutputParameter
#' @param id The unique identifier for this parameter object.
#' @param label A short, human-readable label of this object.
#' @param type Specify valid types of data that may be assigned to this parameter.
#' @param secondaryFiles Provides a pattern or expression specifying files or directories. Only valid when type: File or is an array of items: File.
#' @param streamable A value of true indicates that the file is read or written sequentially without seeking. Only valid when type: File or is an array of items: File.
#' @param glob Pattern to find files relative to the output directory.
#' @param loadContents Read text from globbed file.
#' @param outputEval Evaluate an expression to generate the output value.
#' @param outputSource Specifies one or more workflow parameters that supply the value of to the output parameter.
#' @rdname OutputParam
#' @export
OutputParam <- function(id = "output", label = character(), type = "stdout",
                        secondaryFiles = character(), streamable = logical(),
                        glob = character(), loadContents = logical(), outputEval = character(),
                        outputSource = character()){
    ##if(items %in% c("File", "Directory") && type != "array") stop("type must be array!")
    new("OutputParam",
        id = id,
        label = label,
        type = type,
        secondaryFiles = secondaryFiles,
        streamable = streamable,
        outputBinding = list(glob = glob,
                             loadContents = loadContents,
                             outputEval = outputEval),
        outputSource = outputSource)
}

#' OutputParamList
#' @rdname OutputParamList
#' @export
setClass("OutputParamList", representation(outputs = "SimpleList"),
         prototype = list(elementType = "OutputParam"),
         contains = "SimpleList")

#' OutputParamList
#' #' A list of InputParam
#' @param out The default stdout parameter.
#' @param ... The InputParam objects.
#' @rdname OutputParamList
#' @export
OutputParamList <- function(out = OutputParam(), ...){
    oList <- SimpleList(out, ...)
    names(oList) <- lapply(oList, function(x)x@id)
    new("OutputParamList", outputs = oList)
}

setClassUnion("OutputParamListORlist", c("OutputParamList", "list"))


#' Parameters for CWL
#' @rdname cwlParam
#' @export
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

#' Parameters for CWL
#' The main CWL parameter class and constructor for command tools. More details: https://www.commonwl.org/v1.0/CommandLineTool.html
#' @rdname cwlParam
#' @importFrom S4Vectors SimpleList
#' @param cwlVersion CWL version
#' @param cwlClass "CommandLineTool"
#' @param baseCommand Specifies the program to execute
#' @param requirements Requirements that apply to either the runtime environment or the workflow engine.
#' @param hints Any or a list for the workflow engine.
#' @param arguments Command line bindings which are not directly associated with input parameters.
#' @param inputs A object of `InputParamList`.
#' @param outputs A object of `OutputParamList`.
#' @param stdout Capture the command's standard output stream to a file written to the designated output directory.
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
#' @rdname stepInParam
#' @export
setClass("stepInParam",
         slots = c(id = "character",
                   source = "character",
                   linkMerge = "character",
                   default = "ANY",
                   valueFrom = "character"))
#' stepInParam
#' The input parameter of a workflow step. More details: https://www.commonwl.org/v1.0/Workflow.html#WorkflowStepInput
#' @rdname stepInParam
#' @param id A unique identifier for this workflow input parameter.
#' @param source Specifies one or more workflow parameters that will provide input to the underlying step parameter.
#' @param linkMerge The method to use to merge multiple inbound links into a single array.
#' @param default The default value for this parameter to use if either there is no source field, or the value produced by the source is null.
#' @param valueFrom value from string or expression.
#' @export
stepInParam <- function(id, source = character(), linkMerge = character(), default = character(), valueFrom = character()){
    new("stepInParam",
        id = id, source = source, linkMerge = linkMerge,
        default = default, valueFrom = valueFrom)
}

#' stepInParamList
#' @rdname stepInParamList
#' @export
setClass("stepInParamList", representation(Ins = "SimpleList"),
         prototype = list(elementType = "stepInParam"),
         contains = "SimpleList")

#' stepInParamList
#' @rdname stepInParamList
#' @param ... A list of `stepInParam` objects.
#' @export
stepInParamList <- function(...){
    iList <- SimpleList(...)
    names(iList) <- lapply(iList, function(x)x@id)
    new("stepInParamList", Ins = iList)
}

#' stepParam
#' @rdname stepParam
#' @export
setClass("stepParam",
         slots = c(id = "character",
                   run = "cwlParam",
                   In = "stepInParamList",
                   Out = "list"))
#' stepParam
#' A workflow step parameters. More details: https://www.commonwl.org/v1.0/Workflow.html#WorkflowStep
#' @rdname stepParam
#' @param id The unique identifier for this workflow step.
#' @param run A `cwlParam` object.
#' @param In A `stepInParamList`.
#' @param Out A list of outputs
#' @export
stepParam <- function(id, run = cwlParam(), In = stepInParamList(), Out = list()) {
    new("stepParam",
        id = id,
        run = run,
        In = In,
        Out = Out)
}

#' stepParamList
#' @rdname stepParamList
#' @export
setClass("stepParamList", representation(steps = "SimpleList"),
         prototype = list(elementType = "stepParam"),
         contains = "SimpleList")

#' stepParamList
#' @rdname stepParamList
#' @param ... A list of `stepParam`.
#' @export
stepParamList <- function(...){
    iList <- SimpleList(...)
    names(iList) <- lapply(iList, function(x)x@id)
    new("stepParamList", steps = iList)
}

#' cwlStepParam
#' @rdname cwlStepParam
#' @export
setClass("cwlStepParam",
         contains = "cwlParam",
         slots = c(steps = "stepParamList"),
         prototype = list(steps = stepParamList())
         )

#' cwlStepParam
#' A workflow steps paramter, which connect multiple command line steps into a workflow. More details: stepInParamList.
#' @param cwlVersion CWL version
#' @param cwlClass "Workflow".
#' @param requirements Requirements that apply to either the runtime environment or the workflow engine.
#' @param hints Any or a list for the workflow engine.
#' @param arguments Command line bindings which are not directly associated with input parameters.
#' @param inputs A object of `InputParamList`.
#' @param outputs A object of `OutputParamList`.
#' @param stdout Capture the command's standard output stream to a file written to the designated output directory.
#' @param A list of `stepParamList`.
#' @rdname cwlStepParam
#' @export
cwlStepParam <- function(cwlVersion = "v1.0", cwlClass = "Workflow",
                     requirements = list(),
                     hints = list(), arguments = list(),
                     inputs = InputParamList(), outputs = OutputParamList(),
                     stdout = character(), steps = stepParamList()){
    new("cwlStepParam", cwlVersion = cwlVersion, cwlClass = cwlClass,
        baseCommand = character(), requirements = requirements, hints = hints,
        arguments = arguments, inputs = inputs, outputs = outputs, stdout = stdout,
        steps = steps)
}

# show methods for stepParam
setMethod(show, "stepParam", function(object) {
    cat("  ", object@id, ":\n", sep = "")
    cat("    run: ", paste0(object@id, ".cwl"), "\n", sep = "")
    lapply(object@In@Ins, function(y) {
        cat("    ", paste0(y@id, ": ", y@source), "\n", sep = "")
    })
    cat("    out: ", unlist(object@Out), "\n", sep = "")
})

setMethod(show, "stepParamList", function(object) {
    cat("steps:\n")
    lapply(object@steps, function(x) {
        show(x)
    })
})

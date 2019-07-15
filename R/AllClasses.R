
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
#' Parameters for array inputs. To specify an array parameter, the
#' array definition is nested under the type field with 'type: array'
#' and items defining the valid data types that may appear in the
#' array. More details:
#' https://www.commonwl.org/v1.0/CommandLineTool.html#CommandInputArraySchema
#'
#' @export
#' @rdname InputArrayParam
#' @param label A short description for this object
#' @param type Must be "array".
#' @param items Defines the type of the array elements.
#' @param prefix Command line prefix to add before the value.
#' @param separate If true (default), then the prefix and value must
#'     be added as separate command line arguments; if false, prefix
#'     and value must be concatenated into a single command line
#'     argument.
#' @param itemSeparator Join the array elements into a single string
#'     with separator.
#' @param valueFrom String or Expression.
#' @return An object of class `InputArrayParam`.
#' @examples
#' InputArrayParam(items = "string", prefix="-B=", separate = FALSE)
InputArrayParam <- function(label = "", type = "array",
                            items = character(), prefix = "",
                            separate = TRUE, itemSeparator = character(),
                            valueFrom = character()){
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
setClassUnion("characterORlist", c("character", "list"))

#' InputParam
#' @rdname InputParam
#' @export
setClass("InputParam",
         slots = c(
             id = "character",
             label = "character",
             doc = "character",
             type = "characterORInputArrayParam",
             secondaryFiles = "characterORlist",
             inputBinding = "list",
             default = "ANY",
             value = "ANY"
         ),
         prototype = list(
             id = character(),
             label = character(),
             doc = character(),
             type = character(),
             secondaryFiles = character(),
             inputBinding = list(loadContents = logical(),
                                 position = integer(),
                                 prefix = character(),
                                 separate = logical(),
                                 itemSeparator = character(),
                                 valueFrom = character(),
                                 shellQuote = logical()),
             default = character(),
             value = character())
         )

#' InputParam
#' 
#' parameter for a command tool. More details:
#' https://www.commonwl.org/v1.0/CommandLineTool.html#CommandInputParameter
#' @rdname InputParam
#' @param id The unique identifier for this parameter object.
#' @param label A short, human-readable label of this object.
#' @param type valid types of data that may be assigned to this
#'     parameter.
#' @param doc A documentation string for this type.
#' @param secondaryFiles Only valid when type: File or is an array of
#'     items: File. Provides a pattern or expression specifying files
#'     or directories that must be included alongside the primary
#'     file.
#' @param loadContents Only valid when type: File or is an array of
#'     items: File.
#' @param position The position for this parameter.
#' @param prefix Command line prefix to add before the value.
#' @param separate If true (default), then the prefix and value must
#'     be added as separate command line arguments; if false, prefix
#'     and value must be concatenated into a single command line
#'     argument.
#' @param itemSeparator Join the array elements into a single string
#'     with the elements separated by by itemSeparator.
#' @param valueFrom String or Expression.
#' @param shellQuote If ShellCommandRequirement is in the requirements
#'     for the current command, this controls whether the value is
#'     quoted on the command line (default is true).
#' @param default The default value for this parameter
#' @param value Assigned value for this parameter
#' @export
#' @return An object of class `InputParam`.
#' @examples
#' input1 <- InputParam(id = "sth")
InputParam <- function(id, label= "", type = "string",
                       doc = character(), secondaryFiles = character(),
                       loadContents = logical(), position = 0L, prefix = "",
                       separate = TRUE, itemSeparator = character(),
                       valueFrom = character(), shellQuote = logical(),
                       default = character(), value = character()){
    new("InputParam",
        id = id,
        label = label,
        doc = doc,
        type = type,
        secondaryFiles = secondaryFiles,
        inputBinding = list(loadContents = loadContents,
                            position = as.integer(position),
                            prefix = prefix,
                            separate = separate,
                            itemSeparator = itemSeparator,
                            valueFrom = valueFrom,
                            shellQuote = shellQuote),
        default = default,
        value = value)
}

#' InputParamList
#' @rdname InputParamList
#' @export
setClass("InputParamList",
         ## representation(inputs = "SimpleList"),
         prototype = list(elementType = "InputParam"),
         contains = "SimpleList")

#' InputParamList
#' A list of InputParam
#' @param ... The InputParam objects.
#' @rdname InputParamList
#' @export
#' @return An object of class `InputParamList`.
#' @examples
#' input1 <- InputParam(id = "sth")
#' InputParamList(input1)
InputParamList <- function(...){
    iList <- list(...)
    names(iList) <- lapply(iList, function(x)x@id)
    ## new("InputParamList", inputs = iList)
    new("InputParamList", listData = iList)
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
#' 
#' Parameters for array outputs. More details:
#' https://www.commonwl.org/v1.0/CommandLineTool.html#CommandOutputArraySchema
#' @rdname OutputArrayParam
#' @param label A short, human-readable label of this object.
#' @param type Must be "array".
#' @param items Defines the type of the array elements.
#' @param glob Pattern to find files relative to the output directory.
#' @param loadContents Read text from globbed file.
#' @param outputEval Evaluate an expression to generate the output
#'     value.
#' @export
#' @return An object of class `OutputArrayParam`.
#' @examples
#' b <- OutputParam(id = "b", type = OutputArrayParam(items = "File"), glob = "*.txt")
OutputArrayParam <- function(label= character(), type = "array",
                             items = character(), glob = character(),
                             loadContents = logical(),
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
             secondaryFiles = "characterORlist",
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
#' 
#' An output parameter for a Command Line Tool. More details:
#' https://www.commonwl.org/v1.0/CommandLineTool.html#CommandOutputParameter
#' @param id The unique identifier for this parameter object.
#' @param label A short, human-readable label of this object.
#' @param type Specify valid types of data that may be assigned to
#'     this parameter.
#' @param secondaryFiles Provides a pattern or expression specifying
#'     files or directories. Only valid when type: File or is an array
#'     of items: File.
#' @param streamable A value of true indicates that the file is read
#'     or written sequentially without seeking. Only valid when type:
#'     File or is an array of items: File.
#' @param glob Pattern to find files relative to the output directory.
#' @param loadContents Read text from globbed file.
#' @param outputEval Evaluate an expression to generate the output
#'     value.
#' @param outputSource Specifies one or more workflow parameters that
#'     supply the value of to the output parameter.
#' @rdname OutputParam
#' @export
#' @return An object of class `OutputParam`.
#' @examples
#' o1 <- OutputParam(id = "file", type = "File", glob = "*.txt")
OutputParam <- function(id = "output", label = character(), type = "stdout",
                        secondaryFiles = character(), streamable = logical(),
                        glob = character(), loadContents = logical(),
                        outputEval = character(),
                        outputSource = character()){
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
setClass("OutputParamList",
         ## representation(outputs = "SimpleList"),
         prototype = list(elementType = "OutputParam"),
         contains = "SimpleList")

#' OutputParamList
#' #' A list of InputParam
#' @param out The default stdout parameter.
#' @param ... The InputParam objects.
#' @rdname OutputParamList
#' @export
#' @return An object of class `OutputParamList`.
#' @examples
#' o1 <- OutputParam(id = "file", type = "File", glob = "*.txt")
#' OutputParamList(o1)
OutputParamList <- function(out = OutputParam(), ...){
    oList <- list(out, ...)
    names(oList) <- lapply(oList, function(x)x@id)
    new("OutputParamList", listData = oList)
}

setClassUnion("OutputParamListORlist", c("OutputParamList", "list"))

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
             id = "character",
             label = "character",
             inputs = "InputParamList",
             outputs = "OutputParamListORlist",
             stdout = "character"
         ),
         prototype = list(cwlVersion = character(),
                          cwlClass = character(),
                          baseCommand = character(),
                          requirements = list(),
                          hints = list(),
                          id = character(),
                          label = character(),
                          arguments = list(),
                          inputs = InputParamList(),
                          outputs = OutputParamList(),
                          stdout = character()
         ))

#' Parameters for CWL
#' 
#' The main CWL parameter class and constructor for command
#' tools. More details:
#' https://www.commonwl.org/v1.0/CommandLineTool.html
#' @rdname cwlParam
#' @importFrom S4Vectors SimpleList
#' @import methods
#' @import utils
#' @param cwlVersion CWL version
#' @param cwlClass "CommandLineTool"
#' @param baseCommand Specifies the program to execute
#' @param requirements A list of Requirement lists that apply to
#'     either the runtime environment or the workflow engine.
#' @param hints Any or a list for the workflow engine.
#' @param arguments Command line bindings which are not directly
#'     associated with input parameters.
#' @param id The unique identifier for this process object.
#' @param label A short, human-readable label of this process object.
#' @param inputs A object of `InputParamList`.
#' @param outputs A object of `OutputParamList`.
#' @param stdout Capture the command's standard output stream to a
#'     file written to the designated output directory.
#' @export
#' @details https://www.commonwl.org/v1.0/CommandLineTool.html
#' @return A `cwlParam` class object.
#' @examples
#' input1 <- InputParam(id = "sth")
#' echo <- cwlParam(baseCommand = "echo", inputs = InputParamList(input1))
cwlParam <- function(cwlVersion = "v1.0", cwlClass = "CommandLineTool",
                     baseCommand = character(), requirements = list(),
                     hints = list(), arguments = list(), id = character(),
                     label = character(),
                     inputs = InputParamList(), outputs = OutputParamList(),
                     stdout = character()){
    new("cwlParam", cwlVersion = cwlVersion,
        cwlClass = cwlClass, id = id, label = label,
        baseCommand = baseCommand,
        requirements = requirements, hints = hints,
        arguments = arguments, inputs = inputs,
        outputs = outputs, stdout = stdout)
}

#' stepInParam
#' @rdname stepInParam
#' @export
setClass("stepInParam",
         slots = c(id = "character",
                   source = "characterORlist",
                   linkMerge = "character",
                   default = "ANY",
                   valueFrom = "character"))
#' stepInParam
#' 
#' The input parameter of a workflow step. More details:
#' https://www.commonwl.org/v1.0/Workflow.html#WorkflowStepInput
#' @rdname stepInParam
#' @param id A unique identifier for this workflow input parameter.
#' @param source Specifies one or more workflow parameters that will
#'     provide input to the underlying step parameter.
#' @param linkMerge The method to use to merge multiple inbound links
#'     into a single array.
#' @param default The default value for this parameter to use if
#'     either there is no source field, or the value produced by the
#'     source is null.
#' @param valueFrom value from string or expression.
#' @export
#' @return An object of class `stepInParam`.
#' @examples
#' s1 <- stepInParam(id = "s1")
stepInParam <- function(id, source = character(),
                        linkMerge = character(), default = character(),
                        valueFrom = character()){
    new("stepInParam",
        id = id, source = source, linkMerge = linkMerge,
        default = default, valueFrom = valueFrom)
}

#' stepInParamList
#' @rdname stepInParamList
#' @export
setClass("stepInParamList",
         ## representation(Ins = "SimpleList"),
         prototype = list(elementType = "stepInParam"),
         contains = "SimpleList")

#' stepInParamList
#' @rdname stepInParamList
#' @param ... A list of `stepInParam` objects.
#' @export
#' @return An object of class `stepInParamList`.
#' @examples
#' s1 <- stepInParam(id = "s1")
#' stepInParamList(s1)
stepInParamList <- function(...){
    iList <- list(...)
    ## stopifnot(all(vapply(iList, is, character(1)) == "stepInParam"))
    names(iList) <- lapply(iList, function(x)x@id)
    new("stepInParamList", listData = iList)
}

setClassUnion("cwlParamORcharacter", c("cwlParam", "character"))
#' stepParam
#' @rdname stepParam
#' @export
setClass("stepParam",
         slots = c(id = "character",
                   run = "cwlParamORcharacter",
                   In = "stepInParamList",
                   Out = "list",
                   scatter = "characterORlist",
                   scatterMethod = "character"))
#' stepParam
#' 
#' A workflow step parameters. More details:
#' https://www.commonwl.org/v1.0/Workflow.html#WorkflowStep
#' @rdname stepParam
#' @param id The unique identifier for this workflow step.
#' @param run A `cwlParam` object or the path of a cwl file.
#' @param In A `stepInParamList`.
#' @param Out A list of outputs
#' @param scatter character or a list. The inputs to be scattered.
#' @param scatterMethod required if scatter is an array of more than
#'     one element. It can be one of "dotproduct",
#'     "nested_crossproduct" and "flat_crossproduct". Details:
#'     https://www.commonwl.org/v1.0/Workflow.html#WorkflowStep
#' @export
#' @return An object of class `stepParam`.
#' @examples
#' s1 <- stepParam(id = "s1")
stepParam <- function(id, run = cwlParam(),
                      In = stepInParamList(), Out = list(),
                      scatter = character(), scatterMethod = character()) {
    new("stepParam",
        id = id,
        run = run,
        In = In,
        Out = Out,
        scatter = scatter,
        scatterMethod = scatterMethod)
}

#' stepParamList
#' @rdname stepParamList
#' @export
setClass("stepParamList",
         ## representation(steps = "SimpleList"),
         prototype = list(elementType = "stepParam"),
         contains = "SimpleList")

#' stepParamList
#' @rdname stepParamList
#' @param ... A list of `stepParam`.
#' @export
#' @return An object of class `stepParamList`.
#' @examples
#' s1 <- stepParam(id = "s1")
#' stepParamList(s1)
stepParamList <- function(...){
    iList <- list(...)
    names(iList) <- lapply(iList, function(x)x@id)
    new("stepParamList", listData = iList)
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
#' 
#' A workflow steps paramter, which connect multiple command line
#' steps into a workflow. More details: stepInParamList.
#' @param cwlVersion CWL version
#' @param cwlClass "Workflow".
#' @param requirements Requirements that apply to either the runtime
#'     environment or the workflow engine.
#' @param hints Any or a list for the workflow engine.
#' @param arguments Command line bindings which are not directly
#'     associated with input parameters.
#' @param id The unique identifier for this process object.
#' @param inputs A object of `InputParamList`.
#' @param outputs A object of `OutputParamList`.
#' @param stdout Capture the command's standard output stream to a
#'     file written to the designated output directory.
#' @param steps A list of `stepParamList`.
#' @rdname cwlStepParam
#' @export
#' @return An object of class `cwlStepParam`.
#' @examples
#' input1 <- InputParam(id = "sth")
#' echo1 <- cwlParam(baseCommand = "echo",
#'                   inputs = InputParamList(input1))
#' input2 <- InputParam(id = "sthout", type = "File")
#' echo2 <- cwlParam(baseCommand = "echo",
#'                   inputs = InputParamList(input2),
#'                   stdout = "out.txt")
#' i1 <- InputParam(id = "sth")
#' o1 <- OutputParam(id = "out", type = "File", outputSource = "echo2/output")
#' wf <- cwlStepParam(inputs = InputParamList(i1),
#'                    outputs = OutputParamList(o1))
#' s1 <- Step(id = "echo1", run = echo1, In = list(sth = "sth"))
#' s2 <- Step(id = "echo2", run = echo2, In = list(sthout = "echo1/output"))
#' wf <- wf + s1 + s2
cwlStepParam <- function(cwlVersion = "v1.0", cwlClass = "Workflow",
                     requirements = list(), id = character(),
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
    lapply(object@In, function(y) {
        cat("    ", paste0(y@id, ": ", y@source), "\n", sep = "")
    })
    cat("    out: ", paste(unlist(object@Out), collapse=" "), "\n", sep = "")
    if(length(object@scatter) > 0){
        cat("    scatter:", unlist(object@scatter), "\n", sep = " ")
        if(length(object@scatterMethod) > 0){
            cat("    scatterMethod: ", object@scatterMethod, "\n", sep = "")
        }
    }
})

setMethod(show, "stepParamList", function(object) {
    cat("steps:\n")
    lapply(object, function(x) {
        show(x)
    })
})

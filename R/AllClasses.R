setClassUnion("characterORlist", c("character", "list"))
#' InputArrayParam
#' @rdname InputArrayParam
#' @export
setClass("InputArrayParam",
         slots = c(
             label = "character",
             type = "character",
             items = "characterORlist",
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

setClassUnion("characterORInputArrayParamORlist", c("character", "InputArrayParam", "list"))
#' Input parameters
#' InputParam
#' @rdname InputParam
#' @export
setClass("InputParam",
         slots = c(
             id = "character",
             label = "character",
             doc = "character",
             type = "characterORInputArrayParamORlist",
             secondaryFiles = "characterORlist",
             streamable = "logical",
             format = "character",
             loadContents = "logical",
             loadListing = "character",
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
             streamable = logical(),
             format = character(),
             loadContents = logical(),
             loadListing = character(),
             inputBinding = list(position = integer(),
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
#' @param doc Optional. This argument takes an arbitrary documentation
#'     as a note for this object.
#' @param secondaryFiles Only valid when type: File or is an array of
#'     items: File. Provides a pattern or expression specifying files
#'     or directories that must be included alongside the primary
#'     file.
#' @param streamable Only valid when type: File or is an array of
#'     items: File. A value of true indicates that the file is read or
#'     written sequentially without seeking.
#' @param format Only valid when type: File or is an array of items:
#'     File.
#' @param loadContents Only valid when type: File or is an array of
#'     items: File.
#' @param loadListing Only valid when type: Directory or is an array
#'     of items: Directory.
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
                       streamable = logical(), format = character(),
                       loadListing = character(),
                       loadContents = logical(), position = 0L, prefix = "",
                       separate = TRUE, itemSeparator = character(),
                       valueFrom = character(), shellQuote = logical(),
                       default = character(), value = character()){
    ## init value by type
    if(is(type, "InputArrayParam")){
        value <- list()
    }else{
        if(!is.na(pmatch("int", type))){
            value <- integer()
        }else if(!is.na(pmatch("boolean", type))){
            value <- logical()
        }else if(!is.na(pmatch("float", type))){
            value <- numeric()
        }else if(!is.na(pmatch("int", type))){
            value <- numeric()
        }else if(any(grepl("\\[\\]", type))){
            value <- list()
        }else{
            value <- character()
        }
    }
    new("InputParam",
        id = id,
        label = label,
        doc = doc,
        type = type,
        secondaryFiles = secondaryFiles,
        streamable = streamable,
        format = format,
        loadContents = loadContents,
        loadListing = loadListing,
        inputBinding = list(position = as.integer(position),
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

setClassUnion("characterOROutputArrayParamORlist", c("character", "OutputArrayParam", "list"))

#' Output parameters
#' @rdname OutputParam
#' @export
setClass("OutputParam",
         slots = c(
             id = "character",
             label = "character",
             doc = "character",
             format = "character",
             type = "characterOROutputArrayParamORlist",
             secondaryFiles = "characterORlist",
             streamable = "logical",
             outputBinding = "list",
             outputSource = "character"
         ),
         prototype = list(id = character(),
                          label = character(),
                          doc = character(),
                          format = character(),
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
#' @param doc A documentation string for this object, or an array of
#'     strings which should be concatenated.
#' @param format Only valid when type: File or is an array of items:
#'     File. This is the file format that will be assigned to the
#'     output File object.
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
OutputParam <- function(id = "output", label = character(), doc = character(),
                        type = "stdout",  format = character(),
                        secondaryFiles = character(), streamable = logical(),
                        glob = character(), loadContents = logical(),
                        outputEval = character(),
                        outputSource = character()){
    new("OutputParam",
        id = id,
        label = label,
        doc = doc,
        type = type,
        format = format,
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
setClassUnion("characterORlistORfunction", c("character", "list", "function"))

#' @rdname cwlProcess
#' @export
setClass("cwlProcess",
         slots = c(
             cwlVersion = "character",
             cwlClass = "character",
             baseCommand = "characterORlistORfunction",
             requirements = "list",
             hints = "list",
             arguments = "list",
             id = "character",
             label = "character",
             doc = "characterORlist",
             inputs = "InputParamList",
             outputs = "OutputParamListORlist",
             stdout = "character",
             expression = "character",
             extensions = "list",
             intent = "list"
         ),
         prototype = list(cwlVersion = character(),
                          cwlClass = character(),
                          baseCommand = character(),
                          requirements = list(),
                          hints = list(),
                          id = character(),
                          label = character(),
                          doc = character(),
                          arguments = list(),
                          inputs = InputParamList(),
                          outputs = OutputParamList(),
                          stdout = character(),
                          expression = character(),
                          extensions = list(),
                          intent = list()
         ))

#' Parameters for CWL
#' 
#' The main CWL parameter class and constructor for command
#' tools. More details:
#' https://www.commonwl.org/v1.0/CommandLineTool.html
#' @rdname cwlProcess
#' @importFrom S4Vectors SimpleList
#' @import methods
#' @import utils
#' @param cwlVersion CWL version
#' @param cwlClass "CommandLineTool"
#' @param baseCommand Specifies the program or R function to execute
#' @param requirements A list of requirements that apply to either
#'     the runtime environment or the workflow engine that must be met
#'     in order to execute this process.
#' @param hints Any or a list for the workflow engine.
#' @param arguments Command line bindings which are not directly
#'     associated with input parameters.
#' @param id The unique identifier for this process object.
#' @param label A short, human-readable label of this process object.
#' @param inputs A object of `InputParamList`.
#' @param outputs A object of `OutputParamList`.
#' @param stdout Capture the command's standard output stream to a
#'     file written to the designated output directory.
#' @param expression Javascripts for ExpressionTool class.
#' @param extensions A list of extensions and metadata
#' @param intent An identifier for the type of computational
#'     operation, of this Process.
#' @export
#' @details https://www.commonwl.org/v1.0/CommandLineTool.html
#' @return A `cwlProcess` class object.
#' @examples
#' input1 <- InputParam(id = "sth")
#' echo <- cwlProcess(baseCommand = "echo", inputs = InputParamList(input1))
cwlProcess <- function(cwlVersion = "v1.0",
                     cwlClass = "CommandLineTool",
                     baseCommand = character(), requirements = list(),
                     hints = list(), arguments = list(),
                     id = character(), label = character(),
                     inputs = InputParamList(),
                     outputs = OutputParamList(),
                     stdout = character(), expression = character(),
                     extensions = list(), intent = list()){
    new("cwlProcess", cwlVersion = cwlVersion,
        cwlClass = cwlClass, id = id, label = label,
        baseCommand = baseCommand,
        requirements = requirements, hints = hints,
        arguments = arguments, inputs = inputs,
        outputs = outputs, stdout = stdout,
        expression = expression,
        extensions = extensions,
        intent = intent)
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

setClassUnion("cwlProcessORcharacter", c("cwlProcess", "character"))

#' cwlStep
#' @rdname cwlStep
#' @export
setClass("cwlStep",
         slots = c(id = "character",
                   run = "cwlProcessORcharacter",
                   In = "stepInParamList",
                   Out = "list",
                   scatter = "characterORlist",
                   scatterMethod = "character",
                   label = "character",
                   doc = "character",
                   requirements = "list",
                   hints = "list",
                   when = "character"))

#' cwlStep function
#' @description Constructor function for `cwlStep` object.
#' @param id A user-defined unique identifier for this workflow step.
#' @param run A `cwlProcess` object for command line tool, or path to
#'     a CWL file.
#' @param In A list of input parameters which will be constructed into
#'     `stepInParamList`.
#' @param Out A list of outputs.
#' @param scatter character or a list. The inputs to be scattered.
#' @param scatterMethod required if scatter is an array of more than
#'     one element. It can be one of "dotproduct",
#'     "nested_crossproduct" and "flat_crossproduct".
#' @param label A short, human-readable label of this object.
#' @param doc A documentation string for this object, or an array of
#'     strings which should be concatenated.
#' @param requirements Requirements that apply to either the runtime
#'     environment or the workflow engine.
#' @param hints Hints applying to either the runtime environment or
#'     the workflow engine.
#' @param when If defined, only run the step when the expression
#'     evaluates to true. If false the step is skipped.
#' @export
#' @return An object of class `cwlStep`.
#' @details For more details:
#'     https://www.commonwl.org/v1.0/Workflow.html#WorkflowStep
#' @examples
#' s1 <- cwlStep(id = "s1")
#' @seealso \code{\link{cwlWorkflow}}
cwlStep <- function(id, run = cwlProcess(),
                    In = list(), Out = list(),
                    scatter = character(), scatterMethod = character(),
                    label = character(),
                    doc = character(),
                    requirements = list(),
                    hints = list(),
                    when = character()) {
    if(is(run, "cwlProcess")){
        stopifnot(names(In) %in% names(inputs(run)))
        sout <- as.list(names(outputs(run)))
    }else if(is(run, "character")){
        stopifnot(file.exists(run))
        clist <- read_yaml(run)
        stopifnot(names(In) %in% names(clist$inputs))
        sout <- as.list(names(clist$outputs))
    }
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
    new("cwlStep",
        id = id,
        run = run,
        In = do.call(stepInParamList, slist),
        Out = sout,
        scatter = scatter,
        scatterMethod = scatterMethod,
        label = label,
        doc = doc,
        requirements = requirements,
        hints = hints,
        when = when)
}

#' cwlStepList
#' @rdname cwlStepList
#' @export
setClass("cwlStepList",
         ## representation(steps = "SimpleList"),
         prototype = list(elementType = "cwlStep"),
         contains = "SimpleList")

#' cwlStepList
#' @rdname cwlStepList
#' @param ... A list of `cwlStep`.
#' @export
#' @return An object of class `cwlStepList`.
#' @examples
#' s1 <- cwlStep(id = "s1")
#' cwlStepList(s1)
cwlStepList <- function(...){
    iList <- list(...)
    names(iList) <- lapply(iList, function(x)x@id)
    new("cwlStepList", listData = iList)
}

# show methods for cwlStep
setMethod(show, "cwlStep", function(object) {
    cat(as.yaml(as.listSteps(list(object))))
    ## cat("  ", object@id, ":\n", sep = "")
    ## cat("    run: ", paste0(object@id, ".cwl"), "\n", sep = "")
    ## lapply(object@In, function(y) {
    ##     cat("    ", paste0(y@id, ": ", y@source), "\n", sep = "")
    ## })
    ## cat("    out: ", paste(unlist(object@Out), collapse=" "), "\n", sep = "")
    ## if(length(object@scatter) > 0){
    ##     cat("    scatter:", unlist(object@scatter), "\n", sep = " ")
    ##     if(length(object@scatterMethod) > 0){
    ##         cat("    scatterMethod: ", object@scatterMethod, "\n", sep = "")
    ##     }
    ## }
})

setMethod(show, "cwlStepList", function(object) {
    cat("steps:\n")
    cat(as.yaml(as.listSteps(object)))
    ## lapply(object, function(x) {
    ##     show(x)
    ## })
})




#' cwlWorkflow
#'
#' @rdname cwlWorkflow
#' @export
setClass("cwlWorkflow",
         contains = "cwlProcess",
         slots = c(steps = "cwlStepList"),
         prototype = list(steps = cwlStepList())
         )

setMethod(show, "cwlWorkflow", function(object){
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

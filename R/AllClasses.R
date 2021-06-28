setClassUnion("characterORlist", c("character", "list"))

#' All classes defined in the package of `Rcwl` and the class constructor functions.
#' @rdname AllClasses
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

#' @rdname AllClasses
#' @description InputArrayParam: Parameters for array inputs. To
#'     specify an array parameter, the array definition is nested
#'     under the type field with 'type: array' and items defining the
#'     valid data types that may appear in the array. 
#' @param label A short description for the `InputArrayParam` object.
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
#' @return InputArrayParam: An object of class `InputArrayParam`.
#' @details More details of `InputArrayParam`, see:
#'     https://www.commonwl.org/v1.0/CommandLineTool.html#CommandInputArraySchema
#' @export
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

#' @rdname AllClasses
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

#' @rdname AllClasses
#' @description InputParam: parameter for a command line tool.
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
#' @return An object of class `InputParam`.
#' @details More details for `InputParam`, see:
#'     https://www.commonwl.org/v1.0/CommandLineTool.html#CommandInputParameter
#' @export
#' @examples
#' input1 <- InputParam(id = "sth")
#' InputParamList(input1)
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

setMethod(show, "InputParam", function(object){
    cat(as.yaml(as.listInputs(list(object))))
})

#' @rdname AllClasses
#' @export
setClass("InputParamList",
         ## representation(inputs = "SimpleList"),
         prototype = list(elementType = "InputParam"),
         contains = "SimpleList")

#' @rdname AllClasses
#' @description InputParamList: A list of `InputParam` objects.
#' @param ... The `InputParam` objects.
#' @return InputParamList: An object of class `InputParamList`.
#' @export

InputParamList <- function(...){
    iList <- list(...)
    names(iList) <- lapply(iList, function(x)x@id)
    ## new("InputParamList", inputs = iList)
    new("InputParamList", listData = iList)
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
            if(any(object[[i]]@type@items %in% c("File", "Directory"))){
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
        }else if(is(object[[i]]@type, "list")){
            cat(" ", iname, " (", paste0(unlist(object[[i]]@type),
                                          collapse = "|"), "): \n")
        }
    })
})

#' @rdname AllClasses
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

#' @rdname AllClasses
#' @description OutputArrayParam: Parameters for array outputs.
#' @param label A short, human-readable label of this object.
#' @param type Must be "array".
#' @param items Defines the type of the array elements.
#' @param glob Pattern to find files relative to the output directory.
#' @param loadContents Read text from globbed file.
#' @param outputEval Evaluate an expression to generate the output
#'     value.
#' @details More details for `OutputArrayParam`, see:
#'     https://www.commonwl.org/v1.0/CommandLineTool.html#CommandOutputArraySchema
#' @return An object of class `OutputArrayParam`.
#' @export
#' @examples
#' OutputParam(id = "b", type = OutputArrayParam(items = "File"), glob = "*.txt")
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

#' @rdname AllClasses
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

#' @rdname AllClasses
#' @description OutputParam: An output parameter for a Command Line
#'     Tool.
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
#' @details More details for `OutputParam`, see:
#'     https://www.commonwl.org/v1.0/CommandLineTool.html#CommandOutputParameter
#' @return OutputParam: An object of class `OutputParam`.
#' @export
#' @examples
#' o1 <- OutputParam(id = "file", type = "File", glob = "*.txt")
#' o1
#' 
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

setMethod(show, "OutputParam", function(object){
    cat(as.yaml(as.listOutputs(list(object))))
})

#' @rdname AllClasses
#' @export
setClass("OutputParamList",
         ## representation(outputs = "SimpleList"),
         prototype = list(elementType = "OutputParam"),
         contains = "SimpleList")

#' @rdname AllClasses
#' @description OutputParamList: A list of `InputParam` objects.
#' @param out The default stdout parameter.
#' @param ... The `OutputParam` objects.
#' @return OutputParamList: An object of class `OutputParamList`.
#' @export
#' @examples
#' o1 <- OutputParam(id = "file", type = "File", glob = "*.txt")
#' OutputParamList(o1)
OutputParamList <- function(out = OutputParam(), ...){
    oList <- list(out, ...)
    names(oList) <- lapply(oList, function(x)x@id)
    new("OutputParamList", listData = oList)
}

setMethod(show, "OutputParamList", function(object) {
    cat("outputs:\n")
    cat(as.yaml(as.listOutputs(object)))
})

setClassUnion("OutputParamListORlist", c("OutputParamList", "list"))
setClassUnion("characterORlistORfunction", c("character", "list", "function"))

#' @rdname AllClasses
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
             stdin = "character",
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
                          stdin = character(),
                          expression = character(),
                          extensions = list(),
                          intent = list()
         ))

setMethod(show, "cwlProcess", function(object){
    if(is(baseCommand(object), "function")){
        bc <- c("baseCommand: R function", "\n")
    }else{
        bc <- c("baseCommand:", unlist(baseCommand(object)), "\n")
    }
    cat("class:", class(object), "\n",
        "cwlClass:", cwlClass(object), "\n",
        "cwlVersion:", cwlVersion(object), "\n",
        bc)
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
    if(length(object@stdin) > 0) cat("stdin:", object@stdin, "\n")
    if(length(object@stdout) > 0) cat("stdout:", object@stdout, "\n")
})

#' @rdname AllClasses
#' @export
setClass("stepInParam",
         slots = c(id = "character",
                   source = "characterORlist",
                   linkMerge = "character",
                   default = "ANY",
                   valueFrom = "character"))

#' @rdname AllClasses
#' @description stepInParam: The input parameter of a workflow step.
#' @param id A unique identifier for this workflow input parameter.
#' @param source Specifies one or more workflow parameters that will
#'     provide input to the underlying step parameter.
#' @param linkMerge The method to use to merge multiple inbound links
#'     into a single array.
#' @param default The default value for this parameter to use if
#'     either there is no source field, or the value produced by the
#'     source is null.
#' @param valueFrom value from string or expression.
#' @details More details for `stepInParam`, see:
#'     https://www.commonwl.org/v1.0/Workflow.html#WorkflowStepInput
#' @return stepInParam: An object of class `stepInParam`.
#' @export
#' @examples
#' s1 <- stepInParam(id = "s1")
#' 
stepInParam <- function(id, source = character(),
                        linkMerge = character(), default = character(),
                        valueFrom = character()){
    new("stepInParam",
        id = id, source = source, linkMerge = linkMerge,
        default = default, valueFrom = valueFrom)
}

#' @rdname AllClasses
#' @export
setClass("stepInParamList",
         ## representation(Ins = "SimpleList"),
         prototype = list(elementType = "stepInParam"),
         contains = "SimpleList")

#' @rdname AllClasses
#' @description stepInParamList: A list of `stepInParam` objects.
#' @param ... A list of `stepInParam` objects.
#' @return An object of class `stepInParamList`.
#' @export
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

#' @rdname AllClasses
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

#' @rdname AllClasses
#' @export
setClass("cwlStepList",
         ## representation(steps = "SimpleList"),
         prototype = list(elementType = "cwlStep"),
         contains = "SimpleList")

#' @rdname AllClasses
#' @description cwlStepList: A list of `cwlStep` objects.
#' @param ... A list of `cwlStep` objects.
#' @return cwlStepList: An object of class `cwlStepList`.
#' @export
#' @examples
#' s1 <- cwlStep(id = "s1")
#' cwlStepList(s1)
cwlStepList <- function(...){
    iList <- list(...)
    names(iList) <- lapply(iList, function(x)x@id)
    new("cwlStepList", listData = iList)
}

setMethod(show, "cwlStepList", function(object) {
    cat("steps:\n")
    cat(as.yaml(as.listSteps(object)))
    ## lapply(object, function(x) {
    ##     show(x)
    ## })
})

#' @rdname AllClasses
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

#' cwlProcess methods
#' @name cwlProcess-methods
#' @rdname cwlProcess-methods
#' @description Some useful methods for `cwlProcess` objects.
#' @param cwl A `cwlProcess` (or `cwlWorkflow`) object.
#' @param value A value to be assigned to the `cwlProcess` object.
#' @return cwlVersion: cwl document version
#' @export
#' @examples
#' ip <- InputParam(id = "sth")
#' echo <- cwlProcess(baseCommand = "echo", inputs = InputParamList(ip))
#' cwlVersion(echo)
#' cwlClass(echo)
#' baseCommand(echo)
#' hints(echo)
#' requirements(echo)
#' inputs(echo)
#' outputs(echo)
#' stdOut(echo)
#' extensions(echo)
#' 
#' s1 <- cwlWorkflow()
#' runs(s1)
#' s1
#' short(s1)
 
cwlVersion <- function(cwl) cwl@cwlVersion

#' @rdname cwlProcess-methods
#' @export
"cwlVersion<-" <- function(cwl, value){
    cwl@cwlVersion  <- value
    cwl
}

#' @rdname cwlProcess-methods
#' @return cwlClass: CWL class of `cwlProcess` or `cwlWorkflow` object.
#' @export
cwlClass <- function(cwl) cwl@cwlClass

#' @rdname cwlProcess-methods
#' @export
"cwlClass<-" <- function(cwl, value){
    cwl@cwlClass <- value
    cwl
}

#' @rdname cwlProcess-methods
#' @return baseCommand: base command for the `cwlProcess` object.
#' @export
baseCommand <- function(cwl) cwl@baseCommand

#' @rdname cwlProcess-methods
#' @export
"baseCommand<-" <- function(cwl, value){
    cwl@baseCommand <- value
    cwl
}

#' @rdname cwlProcess-methods
#' @param step the ID of steps in a workflow. This is required if
#'     "cwl" is a `cwlWorkflow` object. It can be multiple levels of
#'     steps separated by "/" for nested workflow.
#' @return arguments: CWL arguments.
#' @export

arguments <- function(cwl, step = NULL){
    if(cwlClass(cwl) == "CommandLineTool"){
        cwl@arguments        
    }else if(cwlClass(cwl) == "Workflow"){
        ## allRun(cwl)[[step]]@arguments
        stopifnot(!is.null(step))
        Steps <- unlist(strsplit(step, split = "/"))
        irun <- "cwl@steps[[ Steps[1] ]]@run"
        if(length(Steps) > 1){
            for(i in 2:length(Steps)){
                irun <- paste0(irun, "@steps[[ Steps[", i, "] ]]@run")
            }
        }
        irun <- paste0(irun, "@arguments")
        eval(parse(text = irun))
    }
}

#' @rdname cwlProcess-methods
#' @export
"arguments<-" <- function(cwl, step = NULL, value){
    if(cwlClass(cwl) == "CommandLineTool"){
        cwl@arguments <- value
    }else if(cwlClass(cwl) == "Workflow"){
        stopifnot(!is.null(step))
        Steps <- unlist(strsplit(step, split = "/"))
        irun <- "cwl@steps[[ Steps[1] ]]@run"
        if(length(Steps) > 1){
            for(i in 2:length(Steps)){
                irun <- paste0(irun, "@steps[[ Steps[", i, "] ]]@run")
            }
        }
        valuec <- as.character(value)
        idxl <- !grepl("^list", valuec)
        if(sum(idxl) > 0){
            valuec[idxl] <- paste0("\"", valuec[idxl], "\"")
        }
        value <- paste(valuec, collapse = ",")
        irun <- paste0(irun, "@arguments <- list(", value, ")")
        eval(parse(text = irun))
    }
    cwl
}

#' @rdname cwlProcess-methods
#' @return hints: CWL hints.
#' @export
hints <- function(cwl) cwl@hints

#' @rdname cwlProcess-methods
#' @export
"hints<-" <- function(cwl, value){
    cwl@hints <- value
    cwl
}

#' @rdname cwlProcess-methods
#' @return requirements: CWL requirments.
#' @export
requirements <- function(cwl, step = NULL){
    if(is.null(step)){
        return(cwl@requirements)
    }else{
        Steps <- unlist(strsplit(step, split = "/"))
        irun <- "cwl@steps[[ Steps[1] ]]@run"
        if(length(Steps) > 1){
            for(i in 2:length(Steps)){
                irun <- paste0(irun, "@steps[[ Steps[", i, "] ]]@run")
            }
        }
        irun <- paste0(irun, "@requirements")
        eval(parse(text = irun))
    }
}

#' @rdname cwlProcess-methods
#' @param step To specifiy a step ID when `cwl` is a workflow.
#' @param value To assign a list of `requirements` value.
#' @export
## "requirements<-" <- function(cwl, value){
##     cwl@requirements <- value
##     cwl
## }
"requirements<-" <- function(cwl, step = NULL, value){
    if(is.null(step)){
        cwl@requirements <- value
    }else{
        Steps <- unlist(strsplit(step, split = "/"))
        irun <- "cwl@steps[[ Steps[1] ]]@run"
        if(length(Steps) > 1){
            for(i in 2:length(Steps)){
                irun <- paste0(irun, "@steps[[ Steps[", i, "] ]]@run")
            }
        }
        valuec <- as.character(value)
        idxl <- !grepl("^list", valuec)
        if(sum(idxl) > 0){
            valuec[idxl] <- paste0("\"", valuec[idxl], "\"")
        }
        value <- paste(valuec, collapse = ",")
        irun <- paste0(irun, "@requirements <- list(", value, ")")
        eval(parse(text = irun))
    }
    cwl
}

#' @rdname cwlProcess-methods
#' @return inputs: A list of `InputParam`.
#' @export

inputs <- function(cwl) cwl@inputs

.assignInput <- function(x, name, value){
    if(!name %in% names(inputs(x)))
        stop(name, " is not in the inputs")
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
            if(length(value) == 0 || value == "")
                stop(name, " is empty!")
            if(!file.exists(value))
                stop(value, " does not exist!")
            v <- list(class = itype, path = normalizePath(value))
        }else if(itype == "File[]"){
            v <- lapply(value, function(x)list(class="File",
                                               path=normalizePath(x)))
        }else{
            v <- value
        }
    }else if(is(itype, "list")){
        if(file_test("-d", value)) {
            v <- list(class = "Directory", path = normalizePath(value))
        }else if(file_test("-f", value)){
            v <- list(class = "File", path = normalizePath(value))
        }else{
            v <- value
        }
    }
    x@inputs[[name]]@value <- v
    x
}

#' @importFrom utils .DollarNames
.DollarNames.cwlProcess <- function(x, pattern = "") {
    grep(pattern, names(inputs(x)), value = TRUE)
}

#' @rdname cwlProcess-methods
#' @description `$`: Extract input values for `cwlProcess`
#'     object. (Can auto-complete the input names using tab)
#' @return `$`: the `InputParam` value for `cwlProcess` object. 
#' @importFrom S4Vectors wmsg
#' @export
#' 
setMethod("$", "cwlProcess", function(x, name){
    if(name %in% names(inputs(x))){
        inputs(x)[[name]]@value
    }else{
        stop(wmsg("the '", name, "' does not exist"))
    }
})

#' @rdname cwlProcess-methods
#' @description `$<-`: Set input values for `cwlProcess` object by name.
#' @param x A `cwlProcess` object.
#' @param name One of input list.
#' @export

setReplaceMethod("$", "cwlProcess", function(x, name, value){
    .assignInput(x, name, value)
})

setGeneric("$")

#' @rdname cwlProcess-methods
#' @description outputs: The outputs of a `cwlProcess` object.
#' @export
#' @return outputs: A list of `OutputParam`.

outputs <- function(cwl) {
    cwl@outputs
}

#' @rdname cwlProcess-methods
#' @description stdOut: stdout of `cwlProcess` object.
#' @return stdOut: CWL stdout.
#' @export
stdOut <- function(cwl) cwl@stdout

#' @rdname cwlProcess-methods
#' @export
"stdOut<-" <- function(cwl, value){
    cwl@stdout <- value
    cwl
}

#' @rdname cwlProcess-methods
#' @description extensions: Extensions and metadata of `cwlProcess` object.
#' @return extensions: A list of extensions or metadata.
#' @export
extensions <- function(cwl) cwl@extensions

#' @rdname cwlProcess-methods
#' @export
"extensions<-" <- function(cwl, value){
    cwl@extensions <- value
    cwl
}

#' @rdname cwlProcess-methods
#' @description short: The function to show a short summary of `cwlProcess` or
#' `cwlWorkflow` object.
#' @return short: A short summary of an object of `cwlProcess` or `cwlWorkflow`.
#' @export

short <- function(cwl){
    if(is(cwl, "cwlProcess")){
        cat(as.yaml(list(inputs = names(inputs(cwl)))))
        cat(as.yaml(list(outputs = names(outputs(cwl)))))
    }
    if(is(cwl, "cwlWorkflow")) {
        cat(as.yaml(list(steps = names(steps(cwl)))))
    }
}

#' cwlWorkflow methods
#' @name cwlWorkflow-methods
#' @rdname cwlWorkflow-methods
#' @description runs: The function to access all runs of a `cwlWorkflow` object.
#' @param object A `cwlWorkflow` object.
#' @return `cwlProcess` objects or paths of CWL file.
#' @export
#' @examples
#' s1 <- cwlWorkflow()
#' runs(s1)
#' s1
#' short(s1)

runs <- function(object){
    stopifnot(is(object, "cwlWorkflow"))
    SimpleList(lapply(steps(object), function(x)x@run))
}

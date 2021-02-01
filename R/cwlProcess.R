#' cwlProcess methods
#' @rdname cwlProcess-methods
#' @export
#' @return cwlVersion: cwl version
cwlVersion <- function(cwl) cwl@cwlVersion

#' cwlVersion
#' CWL document version
#' @export
#' @param cwl A `cwlProcess` object.
#' @param value Assign value to the `cwlProcess` object.
#' @rdname cwlProcess-methods
"cwlVersion<-" <- function(cwl, value){
    cwl@cwlVersion  <- value
    cwl
}

#' cwlClass
#' @rdname cwlProcess-methods
#' @export
#' @return cwlClass: CWL Class
cwlClass <- function(cwl) cwl@cwlClass
#' cwlClass
#' @rdname cwlProcess-methods
#' @export
"cwlClass<-" <- function(cwl, value){
    cwl@cwlClass <- value
    cwl
}

#' baseCommand
#' @rdname cwlProcess-methods
#' @export
#' @return baseCommand: CWL baseCommand
baseCommand <- function(cwl) cwl@baseCommand
#' baseCommand
#' @rdname cwlProcess-methods
#' @export
"baseCommand<-" <- function(cwl, value){
    cwl@baseCommand <- value
    cwl
}

#' arguments
#' @rdname cwlProcess-methods
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
#' arguments
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

#' hints
#' @export
#' @rdname cwlProcess-methods
#' @return hints: CWL hints
hints <- function(cwl) cwl@hints
#' hints
#' @rdname cwlProcess-methods
#' @export
"hints<-" <- function(cwl, value){
    cwl@hints <- value
    cwl
}

#' requirements
#' @rdname cwlProcess-methods
#' @export
#' @return requirements: CWL requirments
## requirements <- function(cwl) cwl@requirements
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

#' requirements
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

#' requireDocker
#' @rdname requirements
#' @param docker The docker pull address.
#' @param Load dockerLoad
#' @param File dockerFile
#' @param Import dockerImport
#' @param ImageId dockerImageId
#' @param OutputDir dockerOutputDirectory
#' @export
#' @return A DockerRequirement list
requireDocker <- function(docker = NULL, Load = NULL, File = NULL,
                          Import = NULL, ImageId = NULL,
                          OutputDir = NULL){
    req <- list(class = "DockerRequirement",
                dockerPull = docker,
                dockerLoad = Load,
                dockerFile = File,
                dockerImport = Import,
                dockerImageId = ImageId,
                dockerOutputDirectory = OutputDir)
    req <- req[lengths(req) > 0]
    return(req)
}

#' requireJS
#' @rdname requirements
#' @param expressionLib optional code
#' @export
#' @return A InlineJavascriptRequirement list
requireJS <- function(expressionLib = list()){
    req <- list(class = "InlineJavascriptRequirement",
                expressionLib = expressionLib)
    req <- req[lengths(req) > 0]
    return(req)
}

#' requireSoftware
#' @rdname requirements
#' @param packages The list of software to be configured.
#' @export
#' @return A SoftwareRequirement list
requireSoftware <- function(packages = list()){
    req <- list(class = "SoftwareRequirement",
                packages = packages)
    req <- req[lengths(req) > 0]
    return(req)
}

#' InitialWorkDirRequirement
#' @rdname requirements
#' @param listing The list of files or directories.
#' @export
#' @return A InitialWorkDirRequirement list
requireInitialWorkDir <- function(listing = list()){
    req <- list(class = "InitialWorkDirRequirement",
                listing = listing)
    req <- req[lengths(req) > 0]
    return(req)
}

#' Dirent class
#'
#' @rdname requirements
#' @param entryname The name of the file or subdirectory to create in
#'     the output directory.
#' @param entry charactor or expression.
#' @param writable boolean.
#' @details https://www.commonwl.org/v1.0/CommandLineTool.html#Dirent
#' @export
Dirent <- function(entryname = character(), entry,
                   writable = FALSE){
    list(entryname = entryname,
         entry = entry,
         writable = writable)
}

#' Create manifest for configure files
#'
#' @rdname requirements
#' @param inputID The input ID from corresponding `InputParam`.
#' @param sep The separator of the input files in the manifest config.
#' @export
#' @examples
#' p1 <- InputParam(id = "ifiles", type = "File[]?", position = -1)
#' CAT = cwlProcess(baseCommand = "cat",
#' requirements = list(requireDocker("alpine"), requireManifest("ifiles"), requireJS()),
#' arguments = list("ifiles"),
#' inputs = InputParamList(p1))
requireManifest <- function(inputID, sep = "\\n"){
    js <- paste0("${var x='';for(var i=0;i<inputs.", inputID,
                 ".length;i++){x+=inputs.", inputID, "[i].path+'",
                 sep, "'}return(x)}")
    
    req1 <- requireInitialWorkDir(
        list(Dirent(entryname = inputID,
                    entry = js)))
    return(req1)
}

#' inputs
#' @rdname InputParamList
#' @param cwl A cwlProcess object
#' @export
#' @return inputs: A list of `InputParam`.
#' @examples
#' ## Inputs
#' input1 <- InputParam(id = "sth")
#' echo <- cwlProcess(baseCommand = "echo", inputs = InputParamList(input1))
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
#' @export
.DollarNames.cwlProcess <- function(x, pattern = "") {
    grep(pattern, names(inputs(x)), value = TRUE)
}

#' Extract input values by name
#' @rdname InputParam
#' @importFrom S4Vectors wmsg
#' @export
setMethod("$", "cwlProcess", function(x, name){
    if(name %in% names(inputs(x))){
        inputs(x)[[name]]@value
    }else{
        stop(wmsg("the '", name, "' does not exist"))
    }
})

#' Set input values by name
#' @param x A `cwlProcess` object.
#' @param name One one of input list
#' @export
#' @rdname InputParam
setReplaceMethod("$", "cwlProcess", function(x, name, value){
    .assignInput(x, name, value)
})

setGeneric("$")

#' outputs
#' The outputs of a cwlProcess object
#' @param cwl A cwlProcess object
#' @rdname OutputParamList
#' @export
#' @return outputs: A list of `OutputParam`.
#' @examples
#' input1 <- InputParam(id = "sth")
#' echo <- cwlProcess(baseCommand = "echo", inputs = InputParamList(input1))
#' outputs(echo) 
outputs <- function(cwl) {
    cwl@outputs
    ## if(is(cwl@outputs, "list")) {
    ##     cwl@outputs
    ## }else{
    ##     cwl@outputs@outputs
    ## }
}

#' stdout of cwlProcess
#' @rdname cwlProcess-methods
#' @export
#' @return stdOut: CWL stdout
stdOut <- function(cwl) cwl@stdout
#' stdout of cwlProcess
#' @rdname cwlProcess-methods
#' @export
"stdOut<-" <- function(cwl, value){
    cwl@stdout <- value
    cwl
}

#' Extensions and metadata of cwlProcess
#' @rdname cwlProcess-methods
#' @return extensions: A list of extensions or metadata
#' @export
extensions <- function(cwl) cwl@extensions
#' @rdname cwlProcess-methods
#' @export
"extensions<-" <- function(cwl, value){
    cwl@extensions <- value
    cwl
}

setMethod(show, "InputParam", function(object){
    cat(as.yaml(as.listInputs(list(object))))
})

setMethod(show, "OutputParam", function(object){
    cat(as.yaml(as.listOutputs(list(object))))
})

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

setMethod(show, "OutputParamList", function(object) {
    cat("outputs:\n")
    cat(as.yaml(as.listOutputs(object)))
})

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
    if(length(object@stdout) > 0) cat("stdout:", object@stdout, "\n")
})

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

#' short
#' 
#' The function to show short summary of cwlProcess or cwlWorkflow
#' @param object An cwlProcess or cwlWorkflow object
#' @export
#' @return A short summary of an object of cwlProcess or cwlWorkflow.
#' @examples
#' s1 <- cwlWorkflow()
#' short(s1)
short <- function(object){
    if(is(object, "cwlProcess")){
        cat(as.yaml(list(inputs = names(inputs(object)))))
        cat(as.yaml(list(outputs = names(outputs(object)))))
    }
    if(is(object, "cwlWorkflow")) {
        cat(as.yaml(list(steps = names(steps(object)))))
    }
}

#' runs
#'
#' The function to access all runs of a cwlWorkflow object
#' @param object A cwlWorkflow object.
#' @export
#' @return cwlProcess objects or paths of CWL file.
#' @examples
#' s1 <- cwlWorkflow()
#' runs(s1)
runs <- function(object){
    stopifnot(is(object, "cwlWorkflow"))
    SimpleList(lapply(steps(object), function(x)x@run))
}

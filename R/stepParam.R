#' workflow class

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
        ## cat("  ", x@id, ":\n", sep = "")
        ## cat("    run: ", paste0(x@id, ".cwl"), "\n", sep = "")
        ## lapply(x@In@Ins, function(y) {
        ##     cat("    ", paste0(y@id, ": ", y@source), "\n", sep = "")
        ## })
        ## cat("    out: ", x@Out, "\n", sep = "")
    })
})

#' Step function
#' @param In one or two layes of list.
#' @export
Step <- function(id, run = cwlParam(), In = list()) {
    stopifnot(names(In) %in% names(inputs(run)))
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
    ##sout <- paste0("[", paste(names(outputs(run)), collapse = ", "), "]")
    sout <- list(names(outputs(run)))
    stepParam(id = id, run = run, In = stepInParamList(slist), Out = sout)
}

#' Pipeline
#' @export
setMethod("+", c("cwlStepParam", "stepParam"), function(e1, e2) {
    pp <- unlist(c(unlist(e1@steps@steps), e2))
    e1@steps <- stepParamList(pp)
    return(e1)
})


## setClass("cwlStepParam",
##          slots = c(
##              cwlVersion = "character",
##              cwlClass = "character",
##              baseCommand = "character",
##              requirements = "list",
##              hints = "list",
##              arguments = "list",
##              inputs = "InputParamList",
##              outputs = "OutputParamList",
##              stdout = "character",
##              steps = "stepParamList"
##          ),
##          prototype = list(cwlVersion = character(),
##                           cwlClass = character(),
##                           baseCommand = character(),
##                           requirements = list(),
##                           hints = list(),
##                           arguments = list(),
##                           inputs = InputParamList(),
##                           outputs = OutputParamList(),
##                           stdout = character(),
##                           steps = stepParamList()
##          ))


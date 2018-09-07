
#' Step function
#' Function to assign value to `stepParam` object.
#' @param id The id of `stepParam` object.
#' @param run A `cwlParam` object for command tool.
#' @param In one or two layes of list.
#' @param scatter character or a list. The inputs to be scattered.
#' @param scatterMethod required if scatter is an array of more than one element. It can be one of "dotproduct", "nested_crossproduct" and "flat_crossproduct". Details: https://www.commonwl.org/v1.0/Workflow.html#WorkflowStep
#' @export
Step <- function(id, run = cwlParam(), In = list(), scatter = character(), scatterMethod = character()) {
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
    stepParam(id = id, run = run, In = stepInParamList(slist), Out = sout,
              scatter = scatter, scatterMethod = scatterMethod)
}

#' Pipeline
#' To build a pipeline by connecting multiple `stepParam` to a `cwlStepParam` object.
#' @param e1 A `cwlStepParam` object.
#' @param e2 A `stepParam` object.
#' @export
setMethod("+", c("cwlStepParam", "stepParam"), function(e1, e2) {
    pp <- unlist(c(unlist(e1@steps@steps), e2))
    e1@steps <- stepParamList(pp)
    return(e1)
})

setGeneric("+")


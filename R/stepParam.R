#' Step function
#' 
#' Function to assign value to `stepParam` object.
#' 
#' @param id The id of `stepParam` object.
#' @param run A `cwlParam` object for command tool, or path to a CWL
#'     file.
#' @param In one or two layes of list.
#' @param scatter character or a list. The inputs to be scattered.
#' @param scatterMethod required if scatter is an array of more than
#'     one element. It can be one of "dotproduct",
#'     "nested_crossproduct" and "flat_crossproduct". Details:
#'     https://www.commonwl.org/v1.0/Workflow.html#WorkflowStep
#' @export
#' @return An object of `stepParam`.
#' @seealso \code{\link{cwlStepParam}}
Step <- function(id, run = cwlParam(), In = list(),
                 scatter = character(), scatterMethod = character()) {
    if(is(run, "cwlParam")){
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
    stepParam(id = id, run = run, In = do.call(stepInParamList, slist), Out = sout,
              scatter = scatter, scatterMethod = scatterMethod)
}

#' Pipeline
#' 
#' To build a pipeline by connecting multiple `stepParam` to a
#' `cwlStepParam` object.
#' @param e1 A `cwlStepParam` object.
#' @param e2 A `stepParam` object.
#' @export
#' @seealso \code{\link{cwlStepParam}}
#' @return A `cwlStepParam` object.
setMethod("+", c("cwlStepParam", "stepParam"), function(e1, e2) {
    ##if (length(e1@steps) == 0) {
    ##    e1@steps <- stepParamList(e2)
    ##} else {
    e1@steps <- do.call(stepParamList, c(e1@steps@listData, e2))
    ##}
    return(e1)
})

setGeneric("+")

#' Steps
#' 
#' Function to extract step slots
#' @param cwl A cwlStepParam object.
#' @export
#' @return steps: A list of stepParam objects.
#' @rdname steps
#' @seealso \code{\link{cwlStepParam}}
steps <- function(cwl) cwl@steps

#' Steps
#' 
#' @export
#' @param value A list of steps.
#' @rdname steps
"steps<-" <- function(cwl, value){
    cwl@steps  <- value
    cwl
}

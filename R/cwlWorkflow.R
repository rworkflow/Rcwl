#' cwlWorkflow function
#'
#' @description The constructor function for `cwlWorkflow` object,
#'     which connects multiple command line steps into a workflow.
#' @param cwlVersion CWL version
#' @param cwlClass "Workflow".
#' @param requirements Requirements that apply to either the runtime
#'     environment or the workflow engine.
#' @param hints Any or a list for the workflow engine.
#' @param extensions A list of extensions and metadata.
#' @param arguments Command line bindings which are not directly
#'     associated with input parameters.
#' @param id A user-defined unique identifier for this workflow.
#' @param label A short, human-readable label of this object.
#' @param doc A documentation string for this object.
#' @param inputs An object of `InputParamList`.
#' @param outputs An object of `OutputParamList`.
#' @param steps A list of `cwlStepList`.
#' @param intent An identifier for the type of computational
#'     operation, of this Process.
#' @export
#' @return cwlWorkflow: An object of class `cwlWorkflow`.
#' @seealso \code{\link{stepInParamList}}
#' @examples
#' input1 <- InputParam(id = "sth")
#' echo1 <- cwlProcess(baseCommand = "echo",
#'                   inputs = InputParamList(input1))
#' input2 <- InputParam(id = "sthout", type = "File")
#' echo2 <- cwlProcess(baseCommand = "echo",
#'                   inputs = InputParamList(input2),
#'                   stdout = "out.txt")
#' i1 <- InputParam(id = "sth")
#' o1 <- OutputParam(id = "out", type = "File", outputSource = "echo2/output")
#' wf <- cwlWorkflow(inputs = InputParamList(i1),
#'                    outputs = OutputParamList(o1))
#' s1 <- cwlStep(id = "echo1", run = echo1, In = list(sth = "sth"))
#' s2 <- cwlStep(id = "echo2", run = echo2, In = list(sthout = "echo1/output"))
#' wf <- wf + s1 + s2
cwlWorkflow <- function(cwlVersion = "v1.0", cwlClass = "Workflow",
                         requirements = list(), id = character(),
                         label = character(), doc = list(), intent = list(),
                         hints = list(), arguments = list(), extensions = list(),
                         inputs = InputParamList(), outputs = OutputParamList(),
                         steps = cwlStepList()){
    new("cwlWorkflow", cwlVersion = cwlVersion, cwlClass = cwlClass,
        requirements = requirements, id = id, label = label,
        doc = doc, intent = intent, hints = hints,
        arguments = arguments, inputs = inputs, outputs = outputs,
        steps = steps, extensions = extensions)
}

#' @rdname cwlWorkflow
#' @param e1 A `cwlWorkflow` object.
#' @param e2 A `cwlStep` object.
#' @export

setMethod("+", c("cwlWorkflow", "cwlStep"), function(e1, e2) {
    ##if (length(e1@steps) == 0) {
    ##    e1@steps <- cwlStepList(e2)
    ##} else {
    e1@steps <- do.call(cwlStepList, c(e1@steps@listData, e2))
    ##}
    return(e1)
})

setGeneric("+")

#' @rdname cwlWorkflow
#' @description steps: Function to extract and assign workflow step
#'     slots.
#' @param cwl A `cwlWorkflow` object.
#' @param value A list of `cwlSteps` to assign.
#' @export
#' @return steps: A list of `cwlStep` objects.
steps <- function(cwl) cwl@steps

#' @rdname cwlWorkflow
#' @export
"steps<-" <- function(cwl, value){
    cwl@steps  <- value
    cwl
}

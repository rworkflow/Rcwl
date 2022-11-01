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
#' @param requirements A list of requirements that apply to either the
#'     runtime environment or the workflow engine that must be met in
#'     order to execute this process.
#' @param hints Any or a list for the workflow engine.
#' @param arguments Command line bindings which are not directly
#'     associated with input parameters.
#' @param id The unique identifier for this process object.
#' @param label A short, human-readable label of this process object.
#' @param doc A documentation string for this object, or an array of
#'     strings which should be concatenated.
#' @param inputs A object of `InputParamList`.
#' @param outputs A object of `OutputParamList`.
#' @param stdout Capture the command's standard output stream to a
#'     file written to the designated output directory.
#' @param stdin A path to a file whose contents must be piped into the
#'     command's standard input stream.
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
                       baseCommand = character(),
                       requirements = list(), hints = list(),
                       arguments = list(), id = character(),
                       label = character(), doc = character(),
                       inputs = InputParamList(),
                       outputs = OutputParamList(),
                       stdout = character(), stdin = character(),
                       expression = character(), extensions = list(),
                       intent = list()){
    new("cwlProcess", cwlVersion = cwlVersion, cwlClass = cwlClass,
        id = id, label = label, baseCommand = baseCommand,
        requirements = requirements, hints = hints, doc = doc,
        arguments = arguments, inputs = inputs, outputs = outputs,
        stdout = stdout, stdin = stdin, expression = expression,
        extensions = extensions, intent = intent)
}

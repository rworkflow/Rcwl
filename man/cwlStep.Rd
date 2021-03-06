% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cwlStep.R
\name{cwlStep}
\alias{cwlStep}
\title{cwlStep function}
\usage{
cwlStep(
  id,
  run = cwlProcess(),
  In = list(),
  Out = list(),
  scatter = character(),
  scatterMethod = character(),
  label = character(),
  doc = character(),
  requirements = list(),
  hints = list(),
  when = character()
)
}
\arguments{
\item{id}{A user-defined unique identifier for this workflow step.}

\item{run}{A `cwlProcess` object for command line tool, or path to
a CWL file.}

\item{In}{A list of input parameters which will be constructed into
`stepInParamList`.}

\item{Out}{A list of outputs.}

\item{scatter}{character or a list. The inputs to be scattered.}

\item{scatterMethod}{required if scatter is an array of more than
one element. It can be one of "dotproduct",
"nested_crossproduct" and "flat_crossproduct".}

\item{label}{A short, human-readable label of this object.}

\item{doc}{A documentation string for this object, or an array of
strings which should be concatenated.}

\item{requirements}{Requirements that apply to either the runtime
environment or the workflow engine.}

\item{hints}{Hints applying to either the runtime environment or
the workflow engine.}

\item{when}{If defined, only run the step when the expression
evaluates to true. If false the step is skipped.}
}
\value{
An object of class `cwlStep`.
}
\description{
Constructor function for `cwlStep` object.
}
\details{
For more details:
    https://www.commonwl.org/v1.0/Workflow.html#WorkflowStep
}
\examples{
s1 <- cwlStep(id = "s1")
}
\seealso{
\code{\link{cwlWorkflow}}
}

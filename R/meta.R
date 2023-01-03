#' @rdname addMeta
#' @return `meta()`: return a list of all available meta information
#'     for the `cwl` object.
#' @export
#' 
meta <- function(cwl){
    idoc <- lapply(cwl@inputs, function(x)list(label = x@label, doc = x@doc))
    odoc <- lapply(cwl@outputs, function(x)list(label = x@label, doc = x@doc))
    if(is(cwl, "cwlWorkflow")){
        sdoc <- lapply(cwl@steps, function(x)list(label = x@label, doc = x@doc))
        metalist <- list(label = cwl@label,
                         doc = cwl@doc,
                         inputs = idoc,
                         outputs = odoc,
                         steps = sdoc,
                         extensions = cwl@extensions)
    }else{
        metalist <- list(label = cwl@label,
                         doc = cwl@doc,
                         inputs = idoc,
                         outputs = odoc,
                         extensions = cwl@extensions)
    }
    return(metalist)
}

#' @rdname addMeta
#' @param value A list of meta information to add to `cwl`.
#' @export
"meta<-" <- function(cwl, value){
    if(!is.null(value$label))
        cwl@label <- value$label    
    if(!is.null(value$doc))
        cwl@doc <- value$doc
    if(!is.null(value$extensions))
        cwl@extensions <- value$extensions

    if(length(value$inputs)>0){
        idx <- match(names(value$inputs), names(inputs(cwl)))
        for(i in idx){
            cwl@inputs[[i]]@doc <- value$inputs[[i]]$doc
            cwl@inputs[[i]]@label <- value$inputs[[i]]$label
        }
    }
    if(length(value$outputs) > 0){
        idx <- match(names(value$outputs), names(outputs(cwl)))
        for(i in idx){
            cwl@outputs[[i]]@doc <- value$outputs[[i]]$doc
            cwl@outputs[[i]]@label <- value$outputs[[i]]$label
        }
    }
    if(length(value$steps) > 0){
        idx <- match(names(value$steps), names(steps(cwl)))
        for(i in idx){
            cwl@steps[[i]]@doc <- value$steps[[i]]$doc
            cwl@steps[[i]]@label <- value$steps[[i]]$label
        }
    }
    return(cwl)
}

#' addMeta
#' Add or change meta information for a cwl recipe.
#' @rdname addMeta
#' @aliases addMeta meta
#' @param cwl `cwlProcess` object for data or tool
#'     recipe. `cwlWorkflow` object for a pipeline recipe.
#' @param label Character string specifying a label for the
#'     recipe. E.g., "bwa align", "gencode annotation".
#' @param doc Character string describing the recipe. E.g,
#'     "Align reads to reference genome".
#' @param inputLabels Vector of character string, specifying labels
#'     for each input parameter.
#' @param inputDocs Vector of character string as descriptions for
#'     each input parameter.
#' @param outputLabels Vector of character string, specifying labels
#'     for each output parameter.
#' @param outputDocs Vector of character string as descriptions for
#'     each output parameter.
#' @param stepLabels Vector of character string, specifying labels for
#'     each step. Use only if `cwl` is a `cwlWorkflow` object.
#' @param stepDocs Vector of character string as description for each
#'     step. Use only if `cwl` is a `cwlWorkflow` object.
#' @param extensions A list of character strings. Can be used to add
#'     meta information about the recipe. Generally, add fields of
#'     information that does not require execution as part of the
#'     recipe evaluation. for information about "author", "url",
#'     "date", "example", use the exact names as list names as shown
#'     in examples, so that they can be correctly passed into
#'     corresponding fields in markdown file when using
#'     `meta2md`. Other information can be added as a list element
#'     with arbitrary names. 
#' @return `addMeta()`: `cwlProcess` or `cwlWorkflow` object, with added meta
#'     information, which can be returned using `meta(cwl)`. Meta
#'     information can be converted into markdown file with `meta2md`
#'     function.
#' @examples
#' \dontrun{
#' library(RcwlPipelines)
#' cwlSearch(c("bwa", "align"))
#' bwaAlign <- RcwlPipelines::cwlLoad("pl_bwaAlign")
#' bwaAlign <- addMeta(
#'   cwl = bwaAlign,
#'   label = "align",
#'   doc = "align reads to reference genome",
#'   inputLabels = c("threads", "readgroup", "reference", "read1", "read2"),
#'   inputDocs = c("number of threads", "read groups",
#'                 "reference genome", "read pair1", "read pair2"),
#'   outputLabels = c("Bam", "Idx"),
#'   outputDocs = c("outputbam file", "index file"),
#'   stepLabels = c(bwa = "bwa"),
#'   stepDocs = c(bwa = "bwa alignment"))
#' cat(meta2md(bwaAlign))
#' }
#'
#' \dontrun{
#' rcp <- ReUseData::recipeLoad("gencode_annotation")
#' meta(rcp)
#' rcp1 <- addMeta(
#'   cwl = rcp,
#'   label = "",
#'   doc = "An empty description line", 
#'   inputLabels = c("input label1", "input label2"),
#'   inputDocs = c("input description 1", "input description 2"), 
#'   outputLabels = c("output label1"),
#'   outputDocs = c("output description 1"), 
#'   extensions = list(
#'     author = "recipe author's name",
#'     url = "http://ftp.ebi.ac.uk/pub/databases/gencode/",
#'     date = as.character(Sys.Date()),
#'     example = "An example"))
#' meta(rcp1)
#' cat(meta2md(rcp1))
#' }
#' @export

addMeta <- function(cwl, label = character(), doc = character(),
                    inputLabels = character(),
                    inputDocs = character(),
                    outputLabels = character(),
                    outputDocs = character(),
                    stepLabels = character(),
                    stepDocs = character(),
                    extensions = list()){
    stopifnot(length(inputLabels) == length(inputDocs))
    stopifnot(length(outputLabels) == length(outputDocs))
    stopifnot(length(stepLabels) == length(stepDocs))
    ## inputs
    ilist <- list()
    for(i in seq(inputLabels)){
        ilist[[i]] <- list(label = inputLabels[i], doc = inputDocs[i])
    }

    if(length(inputs(cwl)) > 0){
        if (all(!is.null(names(inputLabels)))){
            names(ilist) <- names(inputLabels)
        }else if (length(inputLabels) == length(inputs(cwl))){
            names(ilist) <- names(inputs(cwl))
        }
    }
    ## outputs
    olist <- list()
    for(i in seq(outputLabels)){
        olist[[i]] <- list(label = outputLabels[i], doc = outputDocs[i])
    }

    if (all(!is.null(names(outputLabels)))){
        names(olist) <- names(outputLabels)
    }else if (length(outputLabels) == length(outputs(cwl))){
        names(olist) <- names(outputs(cwl))
    }
    ## steps
    slist <- list()
    if(is(cwl, "cwlWorkflow")){
        for(i in seq(stepLabels)){
            slist[[i]] <- list(label = stepLabels[i], doc = stepDocs[i])
        }
        
        if (all(!is.null(names(stepLabels)))){
            names(slist) <- names(stepLabels)
        }else if (length(stepLabels) == length(steps(cwl))){
            names(slist) <- names(steps(cwl))
        }
    }
    meta(cwl) <- list(label = label,
                      doc = doc,
                      inputs = ilist,
                      outputs = olist,
                      steps = slist,
                      extensions = list(`$rud` = extensions))
    return(cwl)
}

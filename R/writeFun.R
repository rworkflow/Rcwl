#' @importFrom R.utils commandArgs
#' @importFrom codetools findGlobals
writeFun <- function(cwl){
    if(length(mvOut@id) > 0){
        file <- file.path(tempdir(), paste0(cwl@id, ".R"))
    }else{
        file <- tempfile("Fun", fileext = ".R")
    }
    funName <- sub(".R", "", basename(file))
    assign(funName, baseCommand(cwl))
    types <- lapply(inputs(cwl), function(x)x@type)
    comArg <- c("suppressPackageStartupMessages(library(R.utils))",
                "suppressPackageStartupMessages(library(codetools))",
                "args <- commandArgs(trailingOnly = TRUE, asValues = TRUE)")
    write(comArg, file = file)

    for(i in seq_along(types)){
        tn <- names(types)[i]
        if(types[[i]] == "int"){
            write(paste0("args[[\"", tn, "\"]] <- as.integer(args[[\"", tn, "\"]])"),
                  file = file, append = TRUE)
        }else if(types[[i]] %in% c("long", "float", "double")){
            write(paste0("args[[\"", tn, "\"]] <- as.numeric(args[[\"", tn, "\"]])"),
                  file = file, append = TRUE)
        }
    }
    ## didn't work
    ff <- findGlobals(baseCommand(cwl))
    ff <- ff[!grepl("package|namespace", sapply(ff, find))]
    if(length(ff) > 0){
        sapply(ff, dump, file = file, append = TRUE)
    }
    dump(funName, file = file, append = TRUE)
    write(paste0("do.call(", funName, ", args)"),
          file = file, append = TRUE)
    return(file)
}

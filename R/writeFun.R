#' @importFrom R.utils commandArgs
#' @importFrom codetools findGlobals
writeFun <- function(cwl, prefix = NULL){
    Dir <- ifelse(is.null(prefix), tempdir(), dirname(prefix))
    Fname <- ifelse(is.null(prefix), basename(tempfile()), basename(prefix))
    if(length(cwl@id) > 0){
        file <- file.path(Dir, paste0(cwl@id, ".R"))
    }else{        
        file <- file.path(Dir, paste0(Fname, ".R"))
    }
    funName <- sub(".R", "", basename(file))
    assign(funName, baseCommand(cwl))
    types <- lapply(inputs(cwl), function(x)x@type)
    ## add user libPaths
    libs <- .libPaths()[1]
    comArg <- c(paste0(".libPaths('", libs, "')"),
                "suppressPackageStartupMessages(library(R.utils))",
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

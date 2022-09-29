#' install udocker
#'
#' To download and install udocker for python3.
#'
#' @importFrom R.utils createLink
#' @export
install_udocker <- function(version = "1.3.4"){
    stopifnot(.Platform$OS.type != "windows")

    if(!file.exists(Sys.which("cwltool"))){
        cl <- basiliskStart(env_Rcwl)
        on.exit(basiliskStop(cl))
    }
    binPath <- Sys.which("cwltool")
    
    download.file(paste0("https://github.com/indigo-dc/udocker/releases/download/v", version, "/udocker-", version, ".tar.gz"),
                  file.path(dirname(binPath), "udocker.tar.gz"))
    untar(file.path(dirname(binPath), "udocker.tar.gz"), exdir = dirname(dirname(binPath)))
    system(paste0(dirname(dirname(binPath)), "/udocker/udocker install"))
    createLink(file.path(dirname(binPath), "udocker"),
               file.path(dirname(dirname(binPath)), "udocker", "udocker"), overwrite = TRUE)
    udocker_path <- Sys.which("udocker")
    message("udocker installed:", "\n", udocker_path)
}

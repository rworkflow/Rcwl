#' install udocker
#'
#' To download and install udocker for python3.
#'
#' @importFrom R.utils createLink
#' @export
install_udocker <- function(){
    stopifnot(.Platform$OS.type != "windows")
    binPath <- system("which cwltool", intern = TRUE)

    download.file("https://github.com/indigo-dc/udocker/releases/download/devel3_1.2.4/udocker-1.2.4.tar.gz",
                  file.path(dirname(binPath), "udocker.tar.gz"))
    untar(file.path(dirname(binPath), "udocker.tar.gz"), exdir = dirname(dirname(binPath)))
    createLink(file.path(dirname(binPath), "udocker"),
               file.path(dirname(dirname(binPath)), "udocker", "udocker"))
    udocker_path <- system("which udocker", intern = TRUE)
    message("udocker installed:", "\n", udocker_path)
}

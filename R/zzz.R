.onLoad <- function(libname, pkgname) {
    cwltool <- system("which cwltool", intern = TRUE)
    if(length(cwltool) > 0){
        cwlversion <- system("cwltool --version", intern = TRUE)
        cwlversion <- tail(strsplit(cwlversion, split = "\\.")[[1]], 1)
        cwlversion <- as.numeric(substring(cwlversion, 1, 4))
        if(cwlversion < 2018){
            warning("Your cwltool is out of date, please update it!")
        }
    }else{
        stop("cwltool is not found, ",
             "Please install cwltool first!\n",
             "https://github.com/common-workflow-language/cwltool#install")
    }
    
    invisible()
}

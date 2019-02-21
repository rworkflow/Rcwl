.onLoad <- function(libname, pkgname) {
    cwltool <- system("which cwltool", intern = TRUE)
    if(length(cwltool) > 0){
        cwlversion <- system("cwltool --version", intern = TRUE)
        cwlversion <- as.numeric(sub('.* 1.', "", cwlversion))
        if(cwlversion < 0.2018){
            stop("Your cwltool is out of date, please update it!")
        }
    }else{
        stop("cwltool is not found, ",
             "Please install cwltool first!\n",
             "https://github.com/common-workflow-language/cwltool#install")
    }
    
    invisible()
}

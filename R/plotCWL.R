#' plotCWL
#'
#' Function to plot cwlStepParam object.
#' @param cwl A cwlStepParam object to plot
#' @param ... other parameters from `mermaid` function
#' @importFrom DiagrammeR mermaid
#' @export
plotCWL <- function(cwl, ...){
    Inputs <- names(inputs(cwl))
    Outputs <- names(outputs(cwl))
    OutSource <- unlist(lapply(outputs(cwl), function(x)x@outputSource))
    
    Steps <- steps(cwl)
    Snames <- names(Steps)
    ## all nodes
    nodes <- c(Inputs, Outputs, Snames)
    if(length(nodes) > 26){
        nn <- length(nodes) - 26
        NodeID <- c(LETTERS, paste0(LETTERS[seq(nn)], seq(nn)))
    }else{
        NodeID <- LETTERS[seq(length(nodes))]
    }
    names(nodes) <- NodeID
    
    mm <- c("graph TB")
    for(i in 1:length(Steps)){
        s1 <- Steps[[i]]
        sid1 <- names(nodes)[match(Snames[i], nodes)]

        ## inputs
        input1 <- unlist(lapply(s1@In@Ins, function(x)x@source))
        mIn <- c()
        for(j in 1:length(input1)){
            if(input1[j] %in% Inputs){
                id1 <- names(nodes)[match(input1[j], nodes)]
                mIn[j] <- paste0(id1, "(", input1[j], ")-->",
                                 sid1, "{", Snames[i], "}")
            }else if(input1[j] %in% OutSource){
                in2os <- names(OutSource)[match(input1[j], OutSource)]
                id1 <- names(nodes)[match(in2os, nodes)]
                mIn[j] <- paste0(id1, "((", in2os, "))-->",
                                 sid1, "{", Snames[i], "}")
            }else{
                ## message("Orphan inputs ", mIn[j])
                sIn <- unlist(strsplit(input1[j], split = "/"))
                id1 <- names(nodes)[match(sIn[1], nodes)]
                mIn[j] <- paste0(id1, "{", sIn[1], "}-->|", sIn[2],
                                 "|", sid1, "{", Snames[i], "}")
            }
        }

        ## outputs
        output1 <- unlist(s1@Out)
        sout1 <- paste(Snames[i], output1, sep = "/")
        mOut <- c()
        for(j in 1:length(output1)){
            if(sout1[j] %in% OutSource){
                o1 <- names(OutSource)[match(sout1[j], OutSource)]
                oid1 <- names(nodes)[match(o1, nodes)]
                mOut[j] <- paste0(sid1, "{", Snames[i], "}", "-->",
                                  oid1, "((", o1, "))")
            }
        }
        mm <- c(mm, mIn, mOut)
    }
    mermaid(paste(mm, collapse = "\n"), ...)
}

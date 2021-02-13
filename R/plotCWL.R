#' plotCWL
#'
#' Function to plot cwlWorkflow object.
#' @param cwl A cwlWorkflow object to plot
#' @param output A string specifying the output type. An option
#'     inherits from `render_graph` and can also be "mermaid".
#' @param layout Layout from `render_graph`.
#' @param ... other parameters from `mermaid` or `render_graph`
#'     function
#' @importFrom DiagrammeR mermaid
#' @importFrom DiagrammeR create_graph
#' @importFrom DiagrammeR add_nodes_from_table
#' @importFrom DiagrammeR add_edges_from_table
#' @importFrom DiagrammeR render_graph
#' @importFrom stats na.omit
#' @export
#' @return A workflow plot.
#' @examples
#' input1 <- InputParam(id = "sth")
#' echo1 <- cwlProcess(baseCommand = "echo",
#'                   inputs = InputParamList(input1))
#' input2 <- InputParam(id = "sthout", type = "File")
#' echo2 <- cwlProcess(baseCommand = "echo",
#'                   inputs = InputParamList(input2),
#'                   stdout = "out.txt")
#' i1 <- InputParam(id = "sth")
#' o1 <- OutputParam(id = "out", type = "File", outputSource = "echo2/output")
#' wf <- cwlWorkflow(inputs = InputParamList(i1),
#'                    outputs = OutputParamList(o1))
#' s1 <- cwlStep(id = "echo1", run = echo1, In = list(sth = "sth"))
#' s2 <- cwlStep(id = "echo2", run = echo2, In = list(sthout = "echo1/output"))
#' wf <- wf + s1 + s2
#' plotCWL(wf)
plotCWL <- function(cwl, output = "graph", layout = "tree", ...){
    if(output == "mermaid"){
        plotMermaid(cwl, ...)
    }else{
        plotGraph(cwl, output = output, layout = layout, ...)
    }
}

plotMermaid <- function(cwl, ...){
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
    for(i in seq_along(Steps)){
        s1 <- Steps[[i]]
        sid1 <- names(nodes)[match(Snames[i], nodes)]

        ## inputs
        input1 <- unlist(lapply(s1@In, function(x)x@source))
        mIn <- c()
        for(j in seq_along(input1)){
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
        for(j in seq_along(output1)){
            if(sout1[j] %in% OutSource){
                o1 <- names(OutSource)[match(sout1[j], OutSource)]
                oid1 <- names(nodes)[match(o1, nodes)]
                mOut[j] <- paste0(sid1, "{", Snames[i], "}", "-->",
                                  oid1, "((", o1, "))")
            }
        }
        mm <- c(mm, mIn, mOut)
    }
    ## remove orphan output nodes
    mm <- na.omit(mm)
    mermaid(paste(mm, collapse = "\n"), ...)
}

plotGraph <- function(cwl, layout = "tree", ...){
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
    shape <- rep(c("rectangle", "circle", "diamond"),
                 c(length(Inputs), length(Outputs), length(Snames)))
    ntable <- data.frame(id = NodeID, label = nodes, shape = shape)
    ntable <- data.frame(ntable, id_ext = ntable$id)
    names(nodes) <- NodeID

    etable <- c()
    for(i in seq_along(Steps)){
        s1 <- Steps[[i]]
        sid1 <- names(nodes)[match(Snames[i], nodes)]
        input1 <- unlist(lapply(s1@In, function(x)x@source))
        mIn <- c()
        for(j in seq_along(input1)){
            if(input1[j] %in% Inputs){
                iid1 <- names(nodes)[match(input1[j], nodes)]
                e1 <- data.frame(from  = iid1, to = sid1)
            }else if(input1[j] %in% OutSource){                
                in2os <- names(OutSource)[match(input1[j], OutSource)]
                iid1 <- names(nodes)[match(in2os, nodes)]
                e1 <- data.frame(from = iid1, to = sid1)
            }else{
                sIn <- unlist(strsplit(input1[j], split = "/"))[1]
                iid1 <- names(nodes)[match(sIn[1], nodes)]
                e1 <- data.frame(from = iid1, to = sid1)
            }
            mIn <- rbind(mIn, e1)
        }

        output1 <- unlist(s1@Out)
        sout1 <- paste(Snames[i], output1, sep = "/")
        mOut <- c()
        for(j in seq_along(output1)){
            if(sout1[j] %in% OutSource){
                o1 <- names(OutSource)[match(sout1[j], OutSource)]
                oid1 <- names(nodes)[match(o1, nodes)]
                mOut <- rbind(mOut, data.frame(from = sid1, to = oid1))
             }
        }
        etable <- rbind(etable, rbind(mIn, mOut))
    }

    graph <- create_graph()
    graph <- add_nodes_from_table(graph, ntable, label_col = "label")
    graph <- add_edges_from_table(graph, etable,
                                  from_col = "from", to_col = "to",
                                  from_to_map = "id_ext")
    render_graph(graph, layout = layout, ...)
}

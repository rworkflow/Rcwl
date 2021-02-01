.inputUI <- function(cwl, inputList, upload){
    ilist <- inputs(cwl)
    dList <- lapply(ilist, function(x){
        if(x@inputBinding$prefix != ""){
            label <- paste0(x@id, " (", x@inputBinding$prefix, " ", x@type, ")")
        }else{
            label <- paste0(x@id, " (", x@type, ")")
        }
        if(x@id %in% names(inputList)){
            d <- selectInput(inputId = x@id,
                             label = label,
                             choices = inputList[[x@id]])
        }else{
            if(is(x@type, "InputArrayParam")){
                d <- textAreaInput(inputId = x@id,
                                   label = label,
                                   value = x@default)
            }else if(x@type == "boolean"){
                d <- selectInput(inputId = x@id,
                                 label = label,
                                 choices = list("TRUE" = TRUE, "FALSE" = FALSE))
            }else if(x@type == "int"){
                v <- ifelse(length(x@default) == 0, NA, x@default)
                d <- numericInput(inputId = x@id,
                                  label = label,
                                  value = v)
            }else if(x@type %in% c("string", "File", "Directory")){
                if(x@type == "File" & upload){
                    d <- fileInput(inputId = x@id,
                                   label = label)
                }else{
                    d <- textInput(inputId = x@id,
                                   label = label,
                                   value = x@default)
                }
            }else if(grepl("\\[", x@type)){
                d <- textAreaInput(inputId = x@id,
                                   label = label,
                                   value = x@default)
            }else{
                d <- textInput(inputId = x@id,
                               label = label,
                               value = x @default)
            }
        }
        
        if(length(x@doc) > 0){
            list(d, helpText(x@doc))
        }else{
            d
        }
    })
    dList
}

#' cwlShiny
#'
#' Function to generate shiny app automaticlly for a `cwlProcess`
#' object.
#' @param cwl A cwlProcess object.
#' @param inputList a list of choices for the inputs of cwl
#'     object. The name of the list must match the inputs of the cwl
#'     object.
#' @param upload Whether to upload file. If FALSE, the upload field
#'     will be text input (file path) instead of file input.
#' @param ... More options for `runCWL`.
#' @import shiny
#' @export
#' @return A shiny webapp.
#' @examples
#' input1 <- InputParam(id = "sth")
#' echo <- cwlProcess(baseCommand = "echo", inputs = InputParamList(input1))
#' echoApp <- cwlShiny(echo)

cwlShiny <- function(cwl, inputList = list(), upload = FALSE, ...){
    stopifnot(is(cwl, "cwlProcess"))
    tList <- titlePanel(cwl@id)
    if(length(cwl@label) > 0) tList <- list(tList, h3(cwl@label))
    divList <- .inputUI(cwl, inputList, upload)

    ui <- fluidPage(                
        tList,
        sidebarLayout(
            sidebarPanel(divList),
            mainPanel(
                actionButton("run", "Run"),
                tabsetPanel(
                    tabPanel("Output", verbatimTextOutput("outPath")),
                    tabPanel("Command", verbatimTextOutput("command")),
                    tabPanel("Log", verbatimTextOutput("log"))
                )
            )
        )
    )

    ilist <- inputs(cwl)
    server <- function(input, output){
        res <- eventReactive(input$run, {
            for(x in names(ilist)){
                if(is(ilist[[x]]@type, "InputArrayParam") ||
                   grepl("\\[", ilist[[x]]@type)){
                    v <- unlist(strsplit(input[[x]], split = ","))
                    eval(parse(text=paste0("cwl$",x," <- v")))
                }else{
                    if(ilist[[x]]@type == "File" & upload){
                        eval(parse(text=paste0("cwl$",x," <- input$", x,
                                               "$datapath")))
                    }else{
                        eval(parse(text=paste0("cwl$",x," <- input$", x)))
                    }
                }
            }
            print("Running...")
            runCWL(cwl, ...)
        })
        output$outPath <- renderText({
            paste(res()$output, collapse = "\n")
        })
        output$command <- renderText({
            paste(res()$command, collapse = "\n")
        })
        output$log <- renderText({
            paste(res()$log, collapse = "\n")
        })}

    shinyApp(ui, server)
}

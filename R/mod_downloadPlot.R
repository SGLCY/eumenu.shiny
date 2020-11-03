#' downloadPlot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param buttonLbel String. The text for the button
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_downloadPlot_ui <- function(id, buttonLabel = "Download plot"){
  ns <- NS(id)
  tagList(
    downloadButton(ns("downloadPlot"), label = buttonLabel, class = "butt"),
    # shinyWidgets::downloadBttn(ns("downloadPlot"), 
    #                            label = buttonLabel,
    #                            style = "minimal", 
    #                            color = "default",
    #                            size = "xs"),
    tags$head(tags$style(".butt{background-color:transparent;} .butt{color: #337ab7;}  .butt{border:0px;}
   .butt{outline:0px;} .butt{font-size:10px;"))
    
  )
}
    
#' downloadPlot Server Function
#' @param plot_name String. A name to be used in the filename when downloading
#' @param the_plot A reactive element. Put it without the ()
#' @param height,width for ggplot::ggsave(). Dimensions of the plot
#' @details This function saves only plots. And specifically reactive plots. Havent tested it for 
#' reactivevalues. Note the () I use in the server part. Not sure why I need this.
#' @noRd 
mod_downloadPlot_server <- function(input, output, session, plot_name, the_plot,width = 10, height = 7){
  ns <- session$ns
 
  # this will create the download handler
  # Remember that the handler's output$_name_ has to be the same as the `downloadButton`'s id
  # in this case its download_plot
  output$downloadPlot <- downloadHandler(
    
    filename = function() {
      paste("plot-",plot_name,".png", sep="")
    },
    
    # See here that i wrapped the `the_plot` with `()`
    content = function(file) {
      ggsave(file, plot = the_plot(), width = width, height = height)
    }
  )
}
    
## To be copied in the UI
# mod_downloadPlot_ui("downloadPlot_ui_1")
    
## To be copied in the server
# callModule(mod_downloadPlot_server, "downloadPlot_ui_1")
 

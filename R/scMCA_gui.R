#' Visualize scMCA result in a web browser.
#'
#' Runs interactive \code{shiny} session of \code{scMCA}
#'
#' @param mca_result an object generate by scMCA funtion
#'
#' @name scMCA_vis
#' @aliases scMCA_vis
#' @import shiny
#' @importFrom  DT dataTableOutput renderDataTable
#' @import dplyr
#' @import  plotly
#' @importFrom  pheatmap pheatmap
#' @importFrom  shinythemes shinytheme
#' @export
scMCA_vis <- function(mca_result){
  ui = tagList(
    navbarPage(
      theme = shinythemes::shinytheme("yeti"),
      "scMCA",
      tabPanel("Visualizer",
               sidebarPanel(
                 h4("Info panal"),
                 sliderInput("slider", "Number:", 1, 20, mca_result$top_cors),
                 h4("plot options"),
                 column(6,
                        sliderInput("col_size", "Cell name size:", 1, 10,4)
                 ),
                 column(6,
                        sliderInput("row_size", "Cell type size:", 1,10,8)
                 ),
                 checkboxInput("checkcells", label = "Show Cells' name", value = TRUE),
                 checkboxInput("checkbar", label = "Show Color Bar", value = TRUE),
                 checkboxInput("checktree", label = "Show cluster tree", value = TRUE)

               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Heatmap",
                            h4("Heatmap out"),
                            plotOutput('heatmapplot')
                   ),
                   tabPanel("plotly",
                            h4("plotly heatmap out"),
                            plotlyOutput("heatmapplotly")
                            ),
                   tabPanel("Result table",
                            h4("result table"),
                            DT::dataTableOutput("mytable")
                            )
                 )
               )
      )
    )
  )
  server = function(input, output) {

    cors_data <- reactive({
      cors_index <- apply(mca_result$cors_matrix,2,gettissue,input$slider)
      cors_index <- sort(unique(as.integer(cors_index)))
      cors_out = reshape2::melt(mca_result$cors_matrix[cors_index,])[c(2,1,3)]
      colnames(cors_out)<- c("Cell","Cell type","Score")
      as.data.frame(cors_out %>% group_by(Cell) %>% top_n(n=input$slider,wt=Score))
    })


    heightSize <- function() {
      length(unique(cors_data()$`Cell type`)) * 10+100
    }
    output$heatmapplot <- renderPlot({
        plotMCA(mca_result,numbers_plot = input$slider, col_font_size=input$col_size,row_font_size=input$row_size,show_col = input$checkcells, show_bar = input$checkbar, show_tree = input$checktree)
    },height = heightSize)

    output$heatmapplotly <- renderPlotly({
      plotMCA(mca_result,numbers_plot = input$slider, col_font_size=input$col_size, row_font_size = input$row_size,interactive_plot = T,show_col = input$checkcells,show_bar = input$checkbar)
    })

    output$mytable = DT::renderDataTable(
      cors_data(),
        extensions = 'Buttons',
      server = FALSE,
      filter = 'top',
      options = list(
        dom = 'Bfrtip',
        buttons =
          list(
            list(
              extend = 'csv',
              buttons = c('csv'),
              exportOptions = list(
                modifiers = list(page = "current")
              )
            ))
      )
    )
  }
  shinyApp(ui=ui, server = server,options = list("launch.browser"=TRUE))
}

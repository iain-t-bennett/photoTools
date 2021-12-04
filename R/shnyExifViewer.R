#' shnyViewer that shows EXIF data for an image
#'
#' @return SHINY APP
#' @export
#'
#' @examples
#' shnyViewer
shnyExifViewer <- function(){
  if (interactive()) {
    options(shiny.maxRequestSize = 20 * 1024^2)
    shiny::shinyApp(ui = ExifViewerUI(), server = ExifViewerServer)
  } else {
    cat("Shiny apps can only be run interactively")
  }
}

# UI for the viewer

ExifViewerUI <- function() {
  shiny::fluidPage(

    # Application title
    shiny::titlePanel("ExifViewer"),

    
    
    # Sidebar with a slider input for number of bins
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput("upload", NULL, buttonLabel = "Upload...", multiple = FALSE)
      ),
      # Show a plot of the generated distribution
      shiny::mainPanel(
        shiny::tableOutput("tabExif")
      )
    )
  )

}

# server for the viewer

ExifViewerServer <- function(input, output) {

  # get the exif data 
  metaExif <- reactive({
    # wait until a file is loaded
    req(input$upload)
    
    # extract the temp filename
    fileName <- input$upload$datapath
    
    # get the exif data for the uploaded file
    exifdf <- getExif(fileName)
    
    exifdf
    
  })
  
  # display as a table
  output$tabExif <- renderTable({
    
    t.mat <- metaExif() %>%
      t()
    
    t.df <- tibble::tibble(Name = rownames(t.mat), Value = t.mat[,1])
    t.df
    
  })
  
}





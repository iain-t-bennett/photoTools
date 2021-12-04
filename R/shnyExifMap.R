#' shnyExifMap shows EXIF data on a google map
#'
#' @return SHINY APP
#' @export
#'
#' @examples
shnyExifMap <- function(){
  if (interactive()) {
    options(shiny.maxRequestSize = 20 * 1024^2)
    shiny::shinyApp(ui = ExifMapUI(apikey = Sys.getenv("GOOGLE_MAPS_APIKEY")), server = ExifMapServer)
  } else {
    cat("Shiny apps can only be run interactively")
  }
}

# UI for the viewer

ExifMapUI <- function(apikey) {
  shiny::fluidPage(

    # Application title
    shiny::titlePanel("Exif Google Maps Viewer"),

    # Sidebar with a slider input for zoom and an upload option
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput("upload", NULL, buttonLabel = "Upload...", multiple = FALSE),
        shiny::textInput("txtapikey", label = "Google Maps API key", value = apikey),
        shiny::sliderInput("sldZoom", label = "Zoom level", min = 1, max = 25, value = 18),
        shiny::tableOutput("tabExif")
      ),
      # Show a plot of the generated distribution
      shiny::mainPanel(
        shiny::plotOutput("pltMap")
      )
    )
  )

}

# server for the viewer

ExifMapServer <- function(input, output) {

  # get the exif data 
  metaExif <- reactive({
    # wait until a file is loaded
    req(input$upload)
    
    # extract the temp filename
    fileName <- input$upload$datapath
    
    # get the exif data for the uploaded file
    rc <- getExif(fileName)
    
    rc
    
  })
  
  # get a map centered on the photo GPS
  basismap <- reactive({
     req(input$upload)
    
     rc <- gmplot(metaExif(), maxzoom = input$sldZoom, api_key = input$txtapikey)
     
     rc
  })
  
  # display the plot
  output$pltMap <- renderPlot({
   rc <- basismap()
   rc
  })
  
  # display as a table
  output$tabExif <- renderTable({
    
    t.mat <- metaExif() %>%
      t()
    
    rc <- tibble::tibble(Name = rownames(t.mat), Value = t.mat[,1])
    
    rc
    
  })
  
}





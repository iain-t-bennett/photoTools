#' shnyViewer that shows EXIF data for an image
#'
#' @return SHINY APP
#' @export
#'
#' @examples
#' shnyViewer
shnyExifViewerLocal <- function(exif_data){
  
  if (interactive()) {
    
    # pass the dataframe so available in app
    server_env <- new.env()
    assign("exif_data", exif_data, server_env)
    this_server <- ExifViewerLocalServer
    this_ui <- ExifViewerLocalUI
    environment(this_server) <- server_env
    environment(this_ui) <- server_env
    
    # launch the app
    shiny::shinyApp(ui = this_ui(apikey = Sys.getenv("GOOGLE_MAPS_APIKEY")), 
                    server = this_server)
    
  } else {
    cat("Shiny apps can only be run interactively")
  }
}

# UI for the viewer

ExifViewerLocalUI <- function(apikey) {
  shiny::fluidPage(

    title = "ExifViewerLocal",
    shiny::hr(),
    shiny::fluidRow(
      shiny::column(4,
             shiny::helpText("Preview"),
             shiny::actionButton("btnBack", "Back"),
             shiny::actionButton("btnNext", "Next"),
             shiny::hr(),
             shiny::imageOutput("imgPrev")
      ),
      shiny::column(4,
             shiny::helpText("Exif Data"),
             shiny::hr(),
             shiny::tableOutput("tabMeta")
      ),
      shiny::column(4,
             shiny::helpText("Location"),
             shiny::textInput("txtapikey", label = "Google Maps API key", value = apikey),
             shiny::sliderInput("sldZoom", label = "Zoom level", min = 1, max = 25, value = 18),
             shiny::hr(),
             shiny::plotOutput("pltMap")
      )
      )
    )
}

# server for the viewer

ExifViewerLocalServer <- function(input, output) {

  # navigation controls
  
  fileIndex <- reactiveVal(1)

  # next
  
  observeEvent(input$btnNext, {
    # update the counter
    newVal <- min(fileIndex() + 1, nrow(exif_data))
    fileIndex(newVal)
  })
  
  # back
  
  observeEvent(input$btnBack, {
    # update the counter
    newVal <- max(fileIndex() - 1, 1)
    fileIndex(newVal)
  })
  
  # preview of image
  
  output$imgPrev <- renderImage({
    
    this.src <- exif_data$SourceFile[fileIndex()]
    this.width <- exif_data$ImageWidth[fileIndex()]
    this.height <- exif_data$ImageHeight[fileIndex()]
  
    displayheight <- floor(400 * (this.height / this.width))
    
    if (exif_data$FileType[fileIndex()] %in% c("JPEG", "PNG", "HEIC", "GIF")){
      
      # Return a list containing the filename
      list(src = this.src,
           contentType = 'image/jpeg',
           width = 400,
           height = displayheight,
           alt = this.src)
    } else {
      list(src = NA,
           alt = "No preview possible")
    }
    
  }, deleteFile = FALSE)
  
  # show the exif data
  
  output$tabMeta <- renderTable({
    
    this.df <- dplyr::slice(exif_data,fileIndex())
    
    t.mat <- this.df %>%
      t()
    
    rc <- tibble(Name = rownames(t.mat), Value = t.mat[,1])
    rc
    
  })
  
  # get a map centered on the photo GPS
  basismap <- reactive({
    req(input$txtapikey)
    
    this.df <- dplyr::slice(exif_data,fileIndex())
    
    # check if coordinates exist
    
    if (!is.na(this.df$GPSLatitude)) {
      rc <- gmplot(this.df, maxzoom = input$sldZoom, api_key = input$txtapikey)   
    } else {
      rc <- ggplot() +
        ggtitle("No GPS locations")
    }
    
    rc
  })
  
  # display the plot
  output$pltMap <- renderPlot({
    rc <- basismap()
    rc
  })
  
}
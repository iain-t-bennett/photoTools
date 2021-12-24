#' Enables modification of exif data
#'
#' @param exif_data The data to modify
#' @param target_path The path to move processed files too
#' @param api_key An api key for google maps
#' @param gmap_data A data frame from prepGMH
#' @param lib_exif A data frame from getExif  
#'
#' @return SHINY APP
#' @export
#'
#' @examples
#' shnyViewer
shnyExifFixGPSLocal <- function(exif_data,
                                target_path,
                                api_key = Sys.getenv("GOOGLE_MAPS_APIKEY"),
                                gmap_data,
                                lib_exif) {
  if (interactive()) {
    
    # if no api key has been set/provided then prompt for it
    if (api_key == ""){
      api_key <- readline(prompt="Please enter google maps api key: ")  
      Sys.setenv(GOOGLE_MAPS_APIKEY=api_key)
    }
    
    # prepare the data
    
    # clean the gmap data from google to only keep likely points
    
    gmap_clean <- gmap_data %>%
      dplyr::filter(Accuracy < 500,!(GPSLatitude == 0 &
                                       GPSLongitude == 0)) %>%
      dplyr::transmute(GPSDate = local_time,
                       Source = "X",
                       GPSLatitude,
                       GPSLongitude)
    
    # find the LOCF and NOCB photos
    # for library only use original GPS points for imputation
    EXIFprep1 <- lib_exif %>%
      dplyr::filter(!is.na(GPSLatitude)) %>%
      dplyr::filter(grepl("_G_", FileName, fixed = TRUE)) %>%
      dplyr::transmute(
        PosixCreateDate,
        SourceFile,
        GPSDate = PosixCreateDate,
        GPSSource = SourceFile,
        GPSLatitude,
        GPSLongitude
      )
    
    # also need the new photos to be sorted
    EXIFprep2 <-  exif_data %>%
      dplyr::transmute(
        PosixCreateDate,
        SourceFile,
        GPSDate = as.POSIXct(ifelse(
          is.na(GPSLatitude), NA, PosixCreateDate
        ), origin = "1970-01-01"),
        GPSSource = ifelse(is.na(GPSLatitude), NA, SourceFile),
        GPSLatitude,
        GPSLongitude
      )
    
    # this will contain empty GPS tags for photos that need imputation
    # plus any known GPS locations based on prior photos
    
    preImpute.df = rbind(EXIFprep1, EXIFprep2) %>%
      dplyr::arrange(PosixCreateDate, SourceFile)
    
    
    # LOCF
    
    LOCF <- preImpute.df %>%
      dplyr::arrange(PosixCreateDate) %>%
      zoo::na.locf(na.rm = FALSE) %>%
      dplyr::transmute(
        SourceFile,
        LOCF_GPSLatitude = GPSLatitude,
        LOCF_GPSLongitude = GPSLongitude,
        LOCF_GPSDate =  as.POSIXct(GPSDate, origin = "1970-01-01"),
        LOCF_GPSSource = GPSSource
      )
    
    # NOCB (do LOCF in reverse)
    
    NOCB <- preImpute.df %>%
      dplyr::arrange(desc(PosixCreateDate)) %>%
      zoo::na.locf(na.rm = FALSE) %>%
      dplyr::transmute(
        SourceFile,
        NOCB_GPSLatitude = GPSLatitude,
        NOCB_GPSLongitude = GPSLongitude,
        NOCB_GPSDate = as.POSIXct(GPSDate, origin = "1970-01-01"),
        NOCB_GPSSource = GPSSource
      )
    
    # combine the imputated data with the raw_exif
    
    exif_data2 <- exif_data %>%
      dplyr::filter(file.exists(SourceFile)) %>%
      dplyr::left_join(LOCF) %>%
      dplyr::left_join(NOCB)
    
    # pass this dataframe so available in app
    # do this by building a new environment to contain the app 
    # and also any data/variables needed there
    
    server_env <- new.env()
    
    # data/variables
    assign("exif_data", exif_data2, server_env)
    assign("target_path", target_path, server_env)
    assign("api_key", api_key, server_env)
    assign("gmap_data", gmap_clean, server_env)
    
    # server/ui functions
    this_server <- ExifFixGPSLocalServer
    this_ui <- ExifFixGPSLocalUI
    environment(this_server) <- server_env
    environment(this_ui) <- server_env
    
    # launch the app
    shiny::shinyApp(ui = this_ui(),
                    server = this_server)
    
  } else {
    cat("Shiny apps can only be run interactively")
  }
}

# UI for the viewer

ExifFixGPSLocalUI <- function() {
  shiny::fluidPage(title = "Location fixer",
                   shiny::fluidRow(
                     shiny::column(
                       8,
                       shiny::textOutput("txtStatus"),
                       shiny::fluidRow(
                         shiny::column(
                           2,
                           shiny::checkboxInput("chkLOCF", value = TRUE, label = "LOCF"),
                           shiny::checkboxInput("chkNOCB", value = TRUE, label = "NOCB"),
                           shiny::checkboxInput("chkGPS", value = TRUE, label = "GPS"),
                           shiny::checkboxInput("chkCUST", value = FALSE, label = "CUST")
                         ),
                         shiny::column(
                           3,
                           shiny::radioButtons("radMode", "Value to write",
                                               choices = c("Original", "LOCF", "NOCB", "Custom"),
                                               selected = "Original"))
                         ,
                         shiny::column(
                           3,
                           shiny::textInput("txtLat", value = "", label = "Lat"),
                           shiny::textInput("txtLon", value = "", label = "Lon")
                         )
                       ),
                       shiny::actionButton("btnWrite", "Write"),
                       shiny::actionButton("btnSkip", "Skip"),
                       shiny::actionButton("btnBack", "Back"),
                       shiny::plotOutput("pltMap", width = 700, height = 700)
                     ),
                     shiny::column(
                       4,
                       shiny::textOutput("txtLOCF"),
                       shiny::imageOutput("imgLOCF", height = "250px"),
                       shiny::textOutput("txtFile"),
                       shiny::imageOutput("imgPre", height = "250px"),
                       shiny::textOutput("txtNOCB"),
                       shiny::imageOutput("imgNOCB", height = "250px")
                     )
                   ))
}

# server for the viewer

ExifFixGPSLocalServer <- function(input, output) {
  
  # reactives for navigation through files
  
  fileIndex <- reactiveVal(1)
  
  output$txtStatus <- renderText({
    paste0(fileIndex(), "/", nrow(exif_data))
  })
  
  # reactive containing GPS coordinates nearby in time to the current file
  # create date time from google maps history
  
  nearGPS <- reactive({
    req(fileIndex())
    this.time <- exif_data$PosixCreateDate[fileIndex()]
    
    gmap_data %>%
      dplyr::transmute(
        targetDate = this.time,
        GPSDate,
        diff = (as.numeric(GPSDate - as.numeric(targetDate))) /
          (60 * 60),
        GPSLatitude,
        GPSLongitude
      ) %>%
      dplyr::filter(abs(diff) < 12)
  })
  
  # reactive containing GPS coordinates to be considered in sizing and 
  # centering the map
  # based on user selections on scope
  
  locations_in_scope <- reactive({
    
    this.df <- dplyr::slice(exif_data, fileIndex())
    
    loc.df <- tibble::tibble(GPSLatitude = 0,
                   GPSLongitude = 0,
                   .rows = 0)
    
    if (!is.na(this.df$GPSLatitude)) {
      loc.df <- this.df %>%
        dplyr::transmute(GPSLatitude, GPSLongitude) %>%
        dplyr::bind_rows(loc.df)
    }
    
    if (!is.na(this.df$LOCF_GPSLatitude) & input$chkLOCF == TRUE) {
      loc.df <- this.df %>%
        dplyr::transmute(GPSLatitude = LOCF_GPSLatitude,
                         GPSLongitude = LOCF_GPSLongitude) %>%
        dplyr::bind_rows(loc.df)
    }
    
    if (!is.na(this.df$NOCB_GPSLatitude) & input$chkNOCB == TRUE) {
      loc.df <- this.df %>%
        dplyr::transmute(GPSLatitude = NOCB_GPSLatitude,
                         GPSLongitude = NOCB_GPSLongitude) %>%
        dplyr::bind_rows(loc.df)
    }
    
    if (input$chkCUST == TRUE) {
      loc.df <- tibble::tibble(
        GPSLatitude = as.numeric(input$txtLat),
        GPSLongitude = as.numeric(input$txtLon)
      ) %>%
        dplyr::bind_rows(loc.df)
    }
    
    loc.df
  })
  
  # preview of the image 
  
  output$imgPre <- renderImage({
    this.src <- exif_data$SourceFile[fileIndex()]
    if (exif_data$FileType[fileIndex()] %in% c("JPEG", "PNG", "HEIC", "GIF")) {
      # Return a list containing the filename
      list(
        src = this.src,
        contentType = 'image/jpeg',
        width = 300,
        height = 250,
        alt = this.src
      )
    } else {
      list(src = NA,
           alt = "VIDEO")
    }
    
  }, deleteFile = FALSE)
  
  # LOCF & NOCB previews 
  
  output$imgLOCF <- renderImage({
    this.src <- exif_data$LOCF_GPSSource[fileIndex()]
    # Return a list containing the filename
    list(
      src = this.src,
      contentType = 'image/jpeg',
      width = 300,
      height = 250,
      alt = this.src
    )
    
  }, deleteFile = FALSE)
  
  output$imgNOCB <- renderImage({
    this.src <- exif_data$NOCB_GPSSource[fileIndex()]
    # Return a list containing the filename
    list(
      src = this.src,
      contentType = 'image/jpeg',
      width = 300,
      height = 250,
      alt = this.src
    )
    
  }, deleteFile = FALSE)
  
  # info captions for the LOCF, NOCB and current photo
  
  output$txtLOCF <- renderText({
    this.file <- exif_data$LOCF_GPSSource[fileIndex()]
    this.lat <- exif_data$LOCF_GPSLatitude[fileIndex()]
    this.long <- exif_data$LOCF_GPSLongitude[fileIndex()]
    paste0(this.file, "\n", this.lat, ",", this.long)
  })
  
  output$txtFile <- renderText({
    this.file <- exif_data$FileName[fileIndex()]
    this.lat <- exif_data$GPSLatitude[fileIndex()]
    this.long <- exif_data$GPSLongitude[fileIndex()]
    paste0(this.file, "\n", this.lat, ",", this.long)
  })
  
  output$txtNOCB <- renderText({
    this.file <- exif_data$NOCB_GPSSource[fileIndex()]
    this.lat <- exif_data$NOCB_GPSLatitude[fileIndex()]
    this.long <- exif_data$NOCB_GPSLongitude[fileIndex()]
    paste0(this.file, "\n", this.lat, ",", this.long)
  })
  
  # reactive of a plot centered on the selected GPS points
  # and scaled to fit the selected points
  
  basismap <- reactive({
    
    loc.df <- locations_in_scope()
    
    if (nrow(loc.df) > 0) {
      rc <- gmplot(loc.df, api_key = api_key)
    } else {
      rc <- ggplot()
    }
    rc
  })
  
  # renders the plot - so can add/remove points without re collecting the 
  # background
  
  output$pltMap <- renderPlot({
    rc <- basismap()
    
    this.df <- dplyr::slice(exif_data, fileIndex())
    
    
    if (!is.na(this.df$GPSLatitude)) {
      rc <- rc +
        ggplot2::geom_hline(aes(yintercept = GPSLatitude, color = "Photo"), data = this.df) +
        ggplot2::geom_vline(aes(xintercept = GPSLongitude, color = "Photo"), data = this.df)
    }
    
    if (!is.na(this.df$LOCF_GPSLatitude)) {
      rc <- rc +
        ggplot2::geom_hline(aes(yintercept = LOCF_GPSLatitude, color = "LOCF"), data = this.df) +
        ggplot2::geom_vline(aes(xintercept = LOCF_GPSLongitude, color = "LOCF"), data = this.df)
    }
    
    if (!is.na(this.df$NOCB_GPSLatitude)) {
      rc <- rc +
        ggplot2::geom_hline(aes(yintercept = NOCB_GPSLatitude, color = "NOCB"), data = this.df) +
        ggplot2::geom_vline(aes(xintercept = NOCB_GPSLongitude, color = "NOCB"), data = this.df)
    }
    
    if (nrow(nearGPS()) > 0) {
      rc <- rc +
        ggplot2::geom_label(aes(y = GPSLatitude, 
                                x = GPSLongitude, 
                                label = round(diff,1)),
                            data = nearGPS())
    }
    
    rc
  })
  
  # the button for write
  # depending on choices sets the file name and some exif tags
  # file names are just appeneded with
  # G for original GPS
  # I for imputed GPS (LOCF/NOCB)
  # M for manually added GPS
  
  observeEvent(input$btnWrite, {
    
    curr.exif <- exif_data %>%
      dplyr::slice(fileIndex())
    
    # build the newExif
    
    new.exif <- curr.exif %>%
      dplyr::mutate(Directory = target_path)
    
    if (input$radMode == "Original"){
      
      new.exif <- new.exif %>%
        dplyr::mutate(FileName = gsub(".","_G.", FileName, fixed = TRUE))
      
    } else if (input$radMode == "LOCF"){
      
      new.exif <- new.exif %>%
        dplyr::mutate(FileName = gsub(".","_I.", FileName, fixed = TRUE),
                      GPSLatitude = LOCF_GPSLatitude,
                      GPSLongitude = LOCF_GPSLongitude)
      
     } else if (input$radMode == "NOCB"){
       
       new.exif <- new.exif %>%
         dplyr::mutate(FileName = gsub(".","_I.", FileName, fixed = TRUE),
                       GPSLatitude = NOCB_GPSLatitude,
                       GPSLongitude = NOCB_GPSLongitude)
       
     } else if (input$radMode == "Custom"){
       
       new.exif <- new.exif %>%
         dplyr::mutate(FileName = gsub(".","_M.", FileName, fixed = TRUE),
                       GPSLatitude = as.numeric(input$txtLat),
                       GPSLongitude = as.numeric(input$txtLon))
       
     }
    
    # fix the latitude/longitude references
    
    new.exif <- new.exif %>%
      dplyr::mutate(GPSLatitudeRef = ifelse(GPSLatitude < 0, "S", "N"),
                    GPSLongitudeRef = ifelse(GPSLongitude < 0, "W", "E"))
    
    # modify the exif and file names
    
    modifyExif(original_exif = curr.exif,
               new_exif = new.exif)
    
    # update the counter
    newVal <- min(fileIndex() + 1, nrow(exif_data))
    
    fileIndex(newVal)
  })
  

  # other navigation buttons 
  # skip - go to the next file without doing anything
  
  observeEvent(input$btnSkip, {
    # update the counter
    newVal <- min(fileIndex() + 1, nrow(exif_data))
    
    fileIndex(newVal)
  })
  
  # as files are physically moved going back will not always work...
  # go back
  
  observeEvent(input$btnBack, {
    # update the counter
    newVal <- max(fileIndex() - 1, 1)
    
    fileIndex(newVal)
  })
  
  
  
  
}

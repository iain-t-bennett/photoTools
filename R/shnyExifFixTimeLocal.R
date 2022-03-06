#' Enables modification of exif data
#'
#' @param exif_data The data to modify
#' @param target_path The path to move processed files too
#' @param existing_filenames Any existing files that should be considered for UID
#'
#' @return SHINY APP
#' @export
#'
#' @examples
#' shnyViewer
shnyExifFixTimeLocal <- function(exif_data, target_path, existing_filenames){
  
  if (interactive()) {
    
    # pass the dataframe so available in app
    server_env <- new.env()
    assign("exif_data", exif_data, server_env)
    assign("target_path", target_path, server_env)
    assign("existing_filenames", existing_filenames, server_env)
    this_server <- ExifFixTimeLocalServer
    this_ui <- ExifFixTimeLocalUI
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

ExifFixTimeLocalUI <- function() {
  shiny::fluidPage(

    title = "Date & Time fixer",
    shiny::hr(),
    shiny::fluidRow(
      shiny::column(4, 
             shiny::textOutput("txtStatus"),
             shiny::textOutput("txtFile"),
             shiny::hr(),
             shiny::textOutput("txtNew")
      ),
      shiny::column(4, 
             shiny::sliderInput("sldShift", label = "Timeshift", min = -12, max = +12, value = 0),
             shiny::uiOutput("uiNew")
      ),
      shiny::column(4, 
             shiny::actionButton("btnWrite", "Write"),
             shiny::actionButton("btnSkip", "Skip"),
             shiny::actionButton("btnBack", "Back"),
      )
    ),
    shiny::hr(),
    shiny::fluidRow(
      shiny::column(4,
             shiny::helpText("Preview"),
             shiny::hr(),
             shiny::imageOutput("imgPrev")
      ),
      shiny::column(4,
             shiny::helpText("Original metadata"),
             shiny::hr(),
             shiny::tableOutput("tabMeta")
      ),
      shiny::column(4,
             shiny::helpText("New metadata"),
             shiny::hr(),
             shiny::tableOutput("tabNewMeta")
      )
    )
  )

}

# server for the viewer

ExifFixTimeLocalServer <- function(input, output) {

  # convert NA to "" for relevant variables
  
  
  existing_files <- tibble::tibble(FileName = existing_filenames,
                          DatePart = substr(FileName, 1,15),
                          UID = substr(FileName, 17,19),
                          id = as.numeric(UID)) 
  
  # navigation controls
  
  fileIndex <- reactiveVal(1)
  
  

  output$txtFile <- renderText({
    this.df <- exif_data %>%
      dplyr::slice(fileIndex())
    this.df$SourceFile
  })
  
  output$tabMeta <- renderTable({
    
    req(exif_data)
    req(fileIndex())
    
    t.mat <- exif_data %>%
      dplyr::slice(fileIndex()) %>%
      dplyr::transmute(CreateDate,ModifyDate,DateTimeOriginal,TrackCreateDate,TrackModifyDate,Make,Model) %>%
      t()
    
    t.df <- tibble::tibble(Name = rownames(t.mat), Value = t.mat[,1])
    t.df
    
  })
  
  
  output$uiNew <- renderUI({
    DTM <- exif_data$CreateDate[fileIndex()]
    DT <- ifelse(DTM=="", format(Sys.Date(), format = "%Y-%m-%d"), gsub(":","-", substr(DTM,1,10)))
    TM <- ifelse(DTM=="", "U",  substr(DTM,12,19))
    MAKE <- exif_data$Make[fileIndex()]
    MODEL <- exif_data$Model[fileIndex()]
    
    MAKE <- ifelse(is.na(MAKE), "", MAKE)
    MODEL <- ifelse(is.na(MODEL), "", MODEL)
    
    if (MAKE=="" & grepl("GOPR",exif_data$SourceFile[fileIndex()])){
      MAKE <- "GoPro"
      MODEL <- "HERO5 Session"
    }
    

    list(
      dateInput("txtDate", label = "Date", value = DT),
      textInput("txtTime", label = "Time", value = TM),
      textInput("txtMake", label = "Make", value = MAKE),
      textInput("txtModel", label = "Model", value = MODEL)
    )
    
    
  })
  
  
  
  new.name <- reactive({
    tm <- input$txtTime
    dt <- input$txtDate
    tm2 <- ifelse(tm %in% c("", "U"), "05:55:55", tm)
    
    dtmO <- paste(format(dt, format ="%Y:%m:%d"), tm2)
    
    dtmN <- as.POSIXct(dtmO, format = "%Y:%m:%d %H:%M:%S")
    
    dtmN2 <- dtmN + input$sldShift*60*60
    
    dtm <- format(dtmN2, format = "%Y:%m:%d %H:%M:%S")
    
    dtN <- format(dtmN2, format = "%Y-%m-%d")
    tmN <- format(dtmN2, format = "%H:%M:%S")
    this.datepart <- "20081018_UUUUUU"
    this.datepart <- paste(gsub("-","", dtN, fixed = TRUE), 
                           ifelse(tm %in% c("", "U"), "UUUUUU", gsub(":","", tmN)),
                           sep = "_")
    
    processed_files <- tibble::tibble(FileName = list.files(target_path),
                              DatePart = substr(FileName, 1,15),
                              UID = substr(FileName, 17,19),
                              id = as.numeric(UID)) %>%
      dplyr::filter(nchar(FileName) > 10) 
    
    ids <- rbind(existing_files, processed_files) %>%
      dplyr::filter(DatePart == this.datepart) 
    
    maxid <- max(ids$id, 0)
    
    this.uid <- formatC(maxid+1,width = 3, flag = "0")
    
    this.filetype <- exif_data$FileTypeExtension[fileIndex()]
    
    paste0(this.datepart, "_", this.uid, ".", this.filetype)
  })
  
  new.exif <- reactive({
    tm <- input$txtTime
    dt <- input$txtDate
    tm2 <- ifelse(tm %in% c("", "U"), "05:55:55", tm)
    
    dtmO <- paste(format(dt, format ="%Y:%m:%d"), tm2)
    
    dtmN <- as.POSIXct(dtmO, format = "%Y:%m:%d %H:%M:%S")
    
    dtmN2 <- dtmN + input$sldShift*60*60
    
    dtm <- format(dtmN2, format = "%Y:%m:%d %H:%M:%S")
    
    
    if (exif_data$FileType[fileIndex()] %in% c("JPEG", "PNG", "HEIC", "GIF")){
      rc <- tibble::tibble(
        CreateDate = dtm,
        ModifyDate = dtm,
        DateTimeOriginal = dtm,
        TrackCreateDate = "",
        TrackModifyDate = "",
        Make = input$txtMake,
        Model = input$txtModel)
    } else {
      rc <- tibble::tibble(
        CreateDate = dtm,
        ModifyDate = dtm,
        DateTimeOriginal = dtm,
        TrackCreateDate = dtm,
        TrackModifyDate = dtm,
        Make = input$txtMake,
        Model = input$txtModel)
    }
    
    rc
    
  })
  
  
  
  output$tabNewMeta <- renderTable({
    
    req(new.exif())
    
    this.df <- new.exif()
    
    t.mat <- this.df %>%
      dplyr::transmute(CreateDate,ModifyDate,DateTimeOriginal,TrackCreateDate,TrackModifyDate,Make,Model) %>%
      t()
    
    t.df <- tibble::tibble(Name = rownames(t.mat), Value = t.mat[,1])
    t.df
    
  })
  
  output$txtStatus <- renderText({
    paste0(fileIndex(), "/", nrow(exif_data))
  })
  
  output$txtNew <- renderText({
    new.name()
  })
  
  output$imgPrev <- renderImage({
    
    this.src <- exif_data$SourceFile[fileIndex()]
    if (exif_data$FileType[fileIndex()] %in% c("JPEG", "PNG", "HEIC", "GIF")){
      
      # Return a list containing the filename
      list(src = this.src,
           contentType = 'image/jpeg',
           width = 400,
           height = 300,
           alt = this.src)
    } else {
      list(src = NA,
           alt = "VIDEO")
    }
    
  }, deleteFile = FALSE)
  
 
  # write button
  
  observeEvent(input$btnWrite, {
    # TODO modify code to go here
    
    df_orig <- exif_data %>%
      dplyr::slice(fileIndex())
    
    # only modify selected tags from orig
    df_new <- df_orig
    
    df_new$FileName <- new.name()
    df_new$Directory <- target_path
    df_new$CreateDate <- new.exif()$CreateDate
    df_new$ModifyDate <- new.exif()$ModifyDate
    df_new$DateTimeOriginal <- new.exif()$DateTimeOriginal
    df_new$TrackCreateDate <- new.exif()$TrackCreateDate
    df_new$TrackModifyDate <- new.exif()$TrackModifyDate
    
    modifyExif(original_exif = df_orig,
               new_exif = df_new)
    
    # update the counter
    newVal <- min(fileIndex() + 1, nrow(exif_data))
    fileIndex(newVal)
    
  })
  
  # skip
  
  observeEvent(input$btnSkip, {
    # update the counter
    newVal <- min(fileIndex() + 1, nrow(exif_data))

    fileIndex(newVal)
  })
  
  # go back
  
  observeEvent(input$btnBack, {
    # update the counter
    newVal <- max(fileIndex() - 1, 1)
    
    fileIndex(newVal)
  })
   
}





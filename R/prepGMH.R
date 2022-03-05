
#' Prepares a location history from google takeout
#'
#' Reference: https://www.chipoglesby.com/2018/03/2018-analyzing-google-location-historyII/
#' @param takeoutjson A "Location History.json" file from google
#'
#' @return tibble containing processed data
#' @export
#'
#' @examples
prepGMH <- function(takeoutjson){
  
  # read the data exported from google maps
  history <- jsonlite::read_json(takeoutjson)
  locs <- history$locations
  
  hist2loc <- function(x){
    null_vec <- rep(NA, 5)
    rc <- c(x$latitudeE7, x$longitudeE7, x$accuracy, x$timestamp, x$source)
    if(length(rc)!=5){
      rc <- null_vec
    } 
    return(rc)
  }
  
  mtrx <- lapply(history$locations, hist2loc) %>%
    unlist() %>%
    matrix(ncol = 5, byrow = TRUE) 
  
  Gmap0 <- tibble::tibble(
                         GPSLatitude = as.numeric(mtrx[,1]) / (10 ^7), 
                         GPSLongitude = as.numeric(mtrx[,2]) / (10 ^7),
                         Accuracy = as.numeric(mtrx[,3]),
                         CreateDateC = mtrx[,4],
                         Source = mtrx[,5]
                         )
  

  # modify to match standards
  Gmap1 <- Gmap0 %>%
    dplyr::filter(!is.na(GPSLatitude),
                  !is.na(GPSLongitude),
                  !is.na(Accuracy),
                  !is.na(CreateDateC),
                  !is.na(Source)) %>%
    dplyr::mutate(CreateDate = as.POSIXct(substr(CreateDateC,1,19), format = "%Y-%m-%dT%H:%M:%OS"),
           AccuracyCat = ifelse(Accuracy < 800, "high", ifelse(Accuracy > 5000, "low", "mid"))
           ) 
  
  # work out the time zone for each point
  Gmap2 <- Gmap1 %>%
    dplyr::mutate(tz = lutz::tz_lookup_coords(lat = GPSLatitude, lon = GPSLongitude)
    )
  
  # find unique timezones
  tzs <- unique(Gmap2$tz)
  
  Gmap3 <- tibble::tibble()
  
  for (this.tz in tzs){
    this.df <- Gmap2 %>%
      dplyr::filter(tz == this.tz) %>%
      dplyr::mutate(off = lutz::tz_offset(CreateDate, tz = this.tz))
    
    Gmap3 <- dplyr::bind_rows(Gmap3, this.df)  
  }
  
  # final process
  
  rc <- Gmap3 %>%
    dplyr::transmute(
      CreateDate,
      GPSLatitude,
      GPSLongitude,
      Accuracy,
      AccuracyCat,
      tz,
      offset = off$utc_offset_h,
      local_time = CreateDate + offset*60*60)

  return(rc)
}


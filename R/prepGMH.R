
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
  
  hist2loc <- function(x){
    rc <- c(x$timestampMs, x$latitudeE7, x$longitudeE7, x$accuracy)
    return(rc)
  }
  
  # get just the timestamps and locations from the data
  mtrx <- lapply(history$locations, hist2loc) %>%
    unlist() %>%
    as.numeric() %>%
    matrix(ncol = 4, byrow = TRUE) 
  
  
  # modify to match standards
  Gmap1 <- tibble::tibble(CreateDate = as.POSIXct(mtrx[,1]/1000, origin = "1970-01-01"),
                          GPSLatitude = mtrx[,2] / (10 ^7),
                          GPSLongitude = mtrx[,3] / (10 ^7),
                          Accuracy = mtrx[,4],
                          AccuracyCat = ifelse(Accuracy < 800, "high", ifelse(Accuracy > 5000, "low", "mid")))
  
  
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


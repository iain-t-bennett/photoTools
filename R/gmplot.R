
#' Title
#'
#' @param coords_to_fit either a vector or a tibble of coordinates named 
#' GPSLatitude, GPSLongitude
#' @param maxzoom api range is from 1 (whole world) to 25 (building)
#' @param api_key api key for access to static maps
#' @param xsize image size to return 
#' @param ysize image size to return
#'
#' @return a ggplot scaled to fit coords_to_fit with a static map background
#' @export
#'
#' @examples
gmplot <- function(coords_to_fit,
                    maxzoom = 18, 
                    api_key = Sys.getenv("GOOGLE_MAPS_APIKEY"),
                    xsize = 600,
                    ysize = 600
                    ){
  
  # if no api key has been set/provided then prompt for it
  if (api_key == ""){
    api_key <- readline(prompt="Please enter google maps api key: ")  
    Sys.setenv(GOOGLE_MAPS_APIKEY=api_key)
  }
  
  # check structure of coords_to_fit and conevrt to a tibble
  # with latitude and longitude
  
  if( "data.frame" %in% class(coords_to_fit)){
    coords <- coords_to_fit %>%
      dplyr::transmute(GPSLatitude, GPSLongitude)
    
  } else if("matrix" %in% class(coords_to_fit)){
    coords <- tibble::tibble(GPSLatitude = coords_to_fit[,1],
                             GPSLongitude = coords_to_fit[,2])
  } else {
    coords <- tibble::tibble(GPSLatitude = coords_to_fit[1],
                             GPSLongitude = coords_to_fit[2])
  }
  
  # from coords to fit work out the zoom needed to fit these
  if(nrow(coords) == 1){
    coord_center <- as.numeric(coords[1,]) 
    est_zoom <- maxzoom
    rng <- tibble::tibble(
      minlat = coord_center[1] - 1,
      maxlat = coord_center[1] + 1,
      minlon = coord_center[2] - 1,
      maxlon = coord_center[2] + 1
    )
    
  } else{
    rng <- coords %>%
      dplyr::summarise(
        minlat = min(GPSLatitude, na.rm = TRUE),
        maxlat = max(GPSLatitude, na.rm = TRUE),
        minlon = min(GPSLongitude, na.rm = TRUE),
        maxlon = max(GPSLongitude, na.rm = TRUE),
        zm = gmestzoom(c(maxlon,maxlat), c(minlon,minlat)),
        cntrlat = geosphere::midPoint(p1 = c(maxlon,maxlat), p2 = c(minlon,minlat))[2],
        cntrlon = geosphere::midPoint(p1 = c(maxlon,maxlat), p2 = c(minlon,minlat))[1]
      )
    coord_center <- c(rng$cntrlat, rng$cntrlon)
    est_zoom <- min(rng$zm, maxzoom)
  }
  
  # build the url
  u_base <- "https://maps.googleapis.com/maps/api/staticmap?"
  u_cntr <- paste0("center=", coord_center[1], ",", coord_center[2])
  u_zoom <- paste0("&zoom=", est_zoom)
  u_size <- paste0("&size=", xsize, "x", ysize)
  u_cfg <- "&maptype=roadmap&scale=2"
  u_key <- paste0("&key=", api_key)
  u_full <- paste0(u_base, u_cntr, u_zoom, u_size, u_cfg, u_key)
  
  # use Rgooglemaps to find boundaries of the tile
  # centers on 0,0 as mid point so take half size to find edges
  
  SW <- RgoogleMaps::XY2LatLon(MyMap = list(lat = coord_center[1], lon = coord_center[2]), 
                               zoom = est_zoom,
                               X = -xsize/2, Y = -ysize/2)
  NE <- RgoogleMaps::XY2LatLon(MyMap = list(lat = coord_center[1], lon = coord_center[2]), 
                               zoom = est_zoom,
                               X = xsize/2, Y = ysize/2)
  
  
  # build an empty pplot woth correct dimensions for tile
  rc <- ggplot() +
    theme_minimal() +
    xlim(c(SW[2], NE[2])) +
    ylim(c(SW[1], NE[1])) +
    xlab("Longitude") +
    ylab("Latitude") +
    coord_quickmap()
  
  # get the static map api response
  resp <- httr::GET(u_full)
  
  # if valid response then add the static map to the empty plot
  if (resp$status_code==200){
  
    # extract the content (should be a png)
    staticmap <- httr::content(resp)
  
    rc <- rc +
      annotation_raster(staticmap, xmin=SW[2], xmax=NE[2], ymin=SW[1], ymax=NE[1]) 
  }
  
  return(rc)
}
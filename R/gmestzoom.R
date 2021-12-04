
#' Estimates zoom parameter for google maps to fit points
#' Calculation source https://stackoverflow.com/questions/6048975/google-maps-v3-how-to-calculate-the-zoom-level-for-a-given-bounds
#' @param ne_coord vector of longitude, latitude for north-east corner of box
#' @param sw_coord vector of longitude, latitude for south-west corner of box
#' @param minzm minimal value to return, deafult of 18
#'
#' @return integer value of zoom to use in a google maps api call
#' @export
#'
#' @examples
gmestzoom <- function(ne_coord, sw_coord){
  # coords as lat , long
  
  latRad <- function (lat) {
    vsin = sin((lat * pi) / 180)
    radX2 = log((1 + vsin) / (1 - vsin)) / 2
    return(max(min(radX2, pi), -pi) / 2)
  }    
  zoom <- function(mapPx, worldPx, fraction) {
    floor(log(mapPx / worldPx / fraction) / log(2));
  }
  latFraction = (latRad(ne_coord[1]) - latRad(sw_coord[1])) / pi
  
  lngDiff = ne_coord[2] - sw_coord[2]
  lngFraction = ifelse(lngDiff < 0, lngDiff + 360 , lngDiff) / 360
  
  latZoom = zoom(640, 256, latFraction)
  lngZoom = zoom(640, 256, lngFraction)
  
  zm <- min(latZoom, lngZoom)
  return(zm)
}

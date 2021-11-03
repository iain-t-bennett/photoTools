
#' Returns selected exifTags and pad any missing tags 
#' 
#' @param files_to_read A vector of files to read
#'
#' @return tibble containing key exif data
#' @export
#'
#' @examples
getExif <- function(files_to_read) {
  
  selected_tags_num <- c(
    'ExifToolVersion',
    'FileSize',
    'ImageWidth',
    'ImageHeight',
    'GPSLatitude',
    'GPSLongitude',
    'GPSAltitudeRef',
    'GPSAltitude'
    )
  
  selected_tags_char <- c(
    'SourceFile',
 
    'FileName',
    'Directory',
    
    'FileModifyDate',

    'FileType',
    'FileTypeExtension',
  
    
    'CreateDate',
    'ModifyDate',
    'DateTimeOriginal',
    'TrackCreateDate',
    'TrackModifyDate',
    

    'GPSLatitudeRef',
    'GPSLongitudeRef',
    
    'GPSPosition',
    
    'GPSDateStamp',
    'GPSTimeStamp',
    'GPSDateTime',
    
    
    'ExifVersion',
    
    'Make',
    'Model'
    
  )
  
  # define a default data frame in case tags are missing
  null_data_num <- as.data.frame(matrix(0,nrow = 0, ncol = length(selected_tags_num)))
  names(null_data_num) <- selected_tags_num
  
  null_data_char <- as.data.frame(matrix("",nrow = 0, ncol = length(selected_tags_char)))
  names(null_data_char) <- selected_tags_char
  
  null_data <- cbind(null_data_char, null_data_num)
  
  # tags to read
  selected_tags <- c(selected_tags_char, selected_tags_num)
  
  # read exif data
  if (length(files_to_read)>0){
    exif_data_raw <- exifr::read_exif(files_to_read,  tags = selected_tags)  
  } 
  
  # expand read tags to all expected and 
  # add posixct dates 
  exif_data <- dplyr::bind_rows(null_data, tibble::as_tibble(exif_data_raw)) %>%
    dplyr::mutate(
      PosixFileModifyDate = as.POSIXct(FileModifyDate, format = "%Y:%m:%d %H:%M:%S"),
      PosixCreateDate = as.POSIXct(CreateDate, format = "%Y:%m:%d %H:%M:%S"),
      PosixModifyDate = as.POSIXct(ModifyDate, format = "%Y:%m:%d %H:%M:%S"),
      PosixDateTimeOriginal = as.POSIXct(DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S"),
      PosixTrackCreateDate = as.POSIXct(TrackCreateDate, format = "%Y:%m:%d %H:%M:%S"),
      PosixTrackModifyDate = as.POSIXct(TrackModifyDate, format = "%Y:%m:%d %H:%M:%S")
    )
  
  return(exif_data)
}
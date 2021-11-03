#' Append an existing exif data set with files that have been updated/added
#'
#' @param path_to_read A vector of directories to search for files
#' @param existing_exif A tibble containing previously read exif data
#'
#' @return
#' @export
#'
#' @examples
getNewExif <- function(files_to_read, existing_exif) {
  
  # get info on the files to read
  info.df <- tibble::tibble(
    SourceFile = files_to_read,
    PosixFileModifyDate = file.mtime(files_to_read),
    FileSize = file.size(files_to_read)
    )
  
  # find any existing exif data 
  valid_exif <- dplyr::inner_join(info.df, existing_exif, 
                                  by = c("SourceFile", "FileSize")) %>%
    dplyr::filter(as.character(PosixFileModifyDate.x) == 
                    as.character(PosixFileModifyDate.y)) %>%
    dplyr::select(-PosixFileModifyDate.x) %>%
    dplyr::rename(PosixFileModifyDate = PosixFileModifyDate.y)
  
  # add any new data needed
  needed_files <- files_to_read[which(!files_to_read  %in% valid_exif$SourceFile)]
  
  needed_exif <- getExif(needed_files)
  
  # combine and return
  exif_data <- dplyr::bind_rows(valid_exif, needed_exif)
  
  return(exif_data)
}
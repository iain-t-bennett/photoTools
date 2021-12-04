
#' Helper function that converts a vector of paths to a vector of files in those directories
#'
#' @param paths_to_read a vector of directories
#'
#' @return a vector of files in the given paths
#' @export
#'
#' @examples
pathtofiles <- function(paths_to_read){
  # get the list of files to consider
  files_list <- list()
  for (i in 1:length(paths_to_read)){
    files_list[[i]] = list.files(paths_to_read[i], recursive = TRUE, full.names = TRUE)  
  }
  files_to_read <- unlist(files_list)
  return(files_to_read)
}
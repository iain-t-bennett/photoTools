#' Applies a set of changes to fix exif data
#'
#' @param original_exif A data frame of exif data
#' @param new_exif A data frame of exif data
#'
#' @return a data frame of status on changes
#' @export
#'
#' @examples
modifyExif <- function(original_exif, new_exif){
  
  assertthat::assert_that(
    nrow(original_exif) == nrow(new_exif),
    msg = "Number of rows must match"
  )

  
  for (file.id in 1:nrow(original_exif)){
  
    this.args <- ""
    
    # check args one be one
    
    this.source <- original_exif$SourceFile
    this.targdir <- new_exif$Directory
    this.tarname <- new_exif$FileName
    this.size <- original_exif$FileSize
    this.target <- paste0(this.targdir, "/", this.tarname)
    
    this.curr <- dplyr::slice(original_exif, file.id) %>%
      dplyr::transmute(CreateDate = ifelse(is.na(CreateDate), "", CreateDate), 
                       ModifyDate = ifelse(is.na(ModifyDate), "", ModifyDate), 
                       DateTimeOriginal = ifelse(is.na(DateTimeOriginal), "", DateTimeOriginal), 
                       TrackCreateDate = ifelse(is.na(TrackCreateDate), "", TrackCreateDate), 
                       TrackModifyDate = ifelse(is.na(TrackModifyDate), "", TrackModifyDate), 
                       Make = ifelse(is.na(Make), "", Make), 
                       Model = ifelse(is.na(Model), "", Model), 
                       GPSLatitude = ifelse(is.na(GPSLatitude), NA, GPSLatitude), 
                       GPSLongitude = ifelse(is.na(GPSLongitude), NA, GPSLongitude), 
                       GPSLatitudeRef = ifelse(is.na(GPSLatitudeRef), "", GPSLatitudeRef), 
                       GPSLongitudeRef = ifelse(is.na(GPSLongitudeRef), "", GPSLongitudeRef)
                       )

    this.new <- dplyr::slice(new_exif, file.id) %>%
      dplyr::transmute(CreateDate = ifelse(is.na(CreateDate), "", CreateDate), 
                       ModifyDate = ifelse(is.na(ModifyDate), "", ModifyDate), 
                       DateTimeOriginal = ifelse(is.na(DateTimeOriginal), "", DateTimeOriginal), 
                       TrackCreateDate = ifelse(is.na(TrackCreateDate), "", TrackCreateDate), 
                       TrackModifyDate = ifelse(is.na(TrackModifyDate), "", TrackModifyDate), 
                       Make = ifelse(is.na(Make), "", Make), 
                       Model = ifelse(is.na(Model), "", Model), 
                       GPSLatitude = ifelse(is.na(GPSLatitude), NA, GPSLatitude), 
                       GPSLongitude = ifelse(is.na(GPSLongitude), NA, GPSLongitude), 
                       GPSLatitudeRef = ifelse(is.na(GPSLatitudeRef), "", GPSLatitudeRef), 
                       GPSLongitudeRef = ifelse(is.na(GPSLongitudeRef), "", GPSLongitudeRef)
                       )
    
    # check for changes
    
    if (!identical(this.curr$CreateDate, this.new$CreateDate)){
      this.args <- paste0(this.args, " -CreateDate='", 
                     ifelse(is.na(this.new$CreateDate),"",this.new$CreateDate)
                     , "'")
    }
    if (!identical(this.curr$ModifyDate, this.new$ModifyDate)){
      this.args <- paste0(this.args, " -ModifyDate='", this.new$ModifyDate, "'")
    }
    if (!identical(this.curr$DateTimeOriginal, this.new$DateTimeOriginal)){
      this.args <- paste0(this.args, " -DateTimeOriginal='", this.new$DateTimeOriginal, "'")
    }
    if (!identical(this.curr$TrackCreateDate, this.new$TrackCreateDate)){
      this.args <- paste0(this.args, " -TrackCreateDate='", this.new$TrackCreateDate, "'")
    }
    if (!identical(this.curr$TrackModifyDate, this.new$TrackModifyDate)){
      this.args <- paste0(this.args, " -TrackModifyDate='", this.new$TrackModifyDate, "'")
    }
    if (!identical(this.curr$Make, this.new$Make)){
      this.args <- paste0(this.args, " -Make='", this.new$Make, "'")
    }
    if (!identical(this.curr$Model, this.new$Model)){
      this.args <- paste0(this.args, " -Model='", this.new$Model, "'")
    } 
    if (!isTRUE(all.equal(this.curr$GPSLatitude, this.new$GPSLatitude, tol = 0.1^8))){
      this.args <- paste0(this.args, " -GPSLatitude='", this.new$GPSLatitude, "'")
    }
    if (!isTRUE(all.equal(this.curr$GPSLongitude, this.new$GPSLongitude, tol = 0.1^8))){
      this.args <- paste0(this.args, " -GPSLongitude='", this.new$GPSLongitude, "'")
    }
    if (!identical(this.curr$GPSLatitudeRef, this.new$GPSLatitudeRef)){
      this.args <- paste0(this.args, " -GPSLatitudeRef='", this.new$GPSLatitudeRef, "'")
    }
    if (!identical(this.curr$GPSLongitudeRef, this.new$GPSLongitudeRef)){
      this.args <- paste0(this.args, " -GPSLongitudeRef='", this.new$GPSLongitudeRef, "'")
    }
    
    cat("\n", this.source, "\n", this.target, "\n", this.args)
    
    if(this.args != ""){
      # at least one change needed
      
      # for large files needed
      # TODO - move this to an installed file with package
      #  this.args <- paste0(
      #    ifelse(file.size(this.source) > 10^7, 
      #           "-config '/Users/iainbenn/Documents/gitloc/PhotoImport/localdata/custom.config' -m ",
      #           "-m "), 
      #    this.args)
      
      this.args <- paste0("-m ", this.args)
      
      # make the change
      
      exifr::exiftool_call(args = this.args, fnames = this.source)
      
      # clean up
      if (file.exists(this.source) & file.exists(paste0(this.source,"_original"))){
        file.remove(paste0(this.source,"_original"))
      }
      
    } # endif at least one change
    
    # copy the files
    
    if(this.source != this.target){
      
      # check that target directory exists
      assertthat::assert_that(
        dir.exists(this.targdir),
        msg = paste0(this.targdir, " does not exist")
      )
      
      # check that target file does not exist already
      assertthat::assert_that(
        !file.exists(this.target),
        msg = paste0(this.target, " already exists")
      )
      
      # do the move
      file.rename(from = this.source, to = this.target)
    }
    
    
    
    
  } # next file loop
  
  
  
}
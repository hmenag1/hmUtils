
#' Checks if a package is loaded
#'
#' @param pack is a string indicating the name of the package to check.
#'
#' @return if the package is loaded then nothing happens. If not, then the package will be loaded.
#' @export
#'
#' @examples
#' checkpack('stats')
checkpack <- function(pack){
  sc <- grep(pack, search())
  if(length(sc)==0){
    lapply(pack, require, character.only=TRUE)
  }
}


#' Calls the file browser window for selecting a file
#'
#'This function asks input from the user for selecting a file
#'
#' @return: List of various information about the file selected by the user. The list contains the path and the name of the selected file.
#' It contains also some basic information about the file including its size and the date and time of file creation.
#'
#' @export
fetchFile <- function(){
  fileInfo <- list(); #browser()
  rawdata <- file.choose(new = FALSE)
  fi <- file.info(rawdata)
  creatime <- fi$ctime
  fp <- rownames(fi)
  fpnew <- gsub("\\\\","/",fp)
  fileInfo["filename"] <- rawdata
  fileInfo[["details"]] <- fi
  fileInfo["filepathR"] <- fpnew
  fileInfo[["createTime"]] <- as.POSIXlt(creatime,origin="1970-01-01")
  return(fileInfo)
}

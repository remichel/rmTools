
#' computername
#' @description \code{computername}
#' Returns current computername.
#' @return character
#' @export computername
#'
#'

computername <- function(){
  name <- as.character(Sys.info()["nodename"])
  return(name)
}

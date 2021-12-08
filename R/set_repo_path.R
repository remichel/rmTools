#' set_repo_path
#'
#' @param ... (Multiple) vectors of string pairs, where the first string is always the computername and the second string the desired repo path for that pc. Van also be list of vectors.
#'
#' @return character
#' @export set_repo_path
#'
set_repo_path <- function(...){

  # unpack ...
  vars = list(...)

  # check if input was already a list
  if(is.list(vars[[1]])){
    vars = vars[[1]]
  }

  # browse computer names
  for(i in 1:length(vars)){
    counter = 0
    if(rmTools::computername() == vars[[i]][1]){
      counter = counter + 1
      pcname = vars[[i]][1]
      repo_path = vars[[i]][2]
      break
    }
  }

  # return repo name
  if(counter == 1){
    message("Computer identified as ", pcname, '. Will set repository path to ', repo_path, '.')
    return(repo_path)
  }else if(counter > 1){
    stop("Found more than 1 matching name. Unable to return repo_path.")
  }else if(counter == 0){
    stop("PCName ", rmTools::computername(), ' seems not to be amongst the listed computernames. Unable to return repo_path.')
  }


}


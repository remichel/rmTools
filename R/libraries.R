##' @title  Load multiple libraries with one command
##' @description loads multiple libraries in one line without massive printing
##' to console.
##' @param ... comma-separated library names without or with quotes. Or a
##' character vector of library names.
##' @return Nothing. Unloadable packs will throw errors.
##' @seealso \code{library()}
##' @examples
##' wmR::libraries(data.table, lme4, ggplot2)
##' wmR::libraries('data.table', 'lme4', 'ggplot2')
##' packs <- c('data.table', 'lme4', 'ggplot2')
##' wmR::libraries(packs)
##'
##' @author Wanja Mössing
##' @name libraries
##' @export libraries
##'
libraries <- function(...){
  packs <- as.character(as.list(substitute(list(...)))[-1L])
  # 'packs' will be nonsense if a vector was entered...
  if (length(packs) == 1 &&
      (substr(packs, 1, 2) == 'c(' | grepl('packs', packs))) {
    packs <- c(...)
  }
  invisible(sapply(packs, library, character.only = TRUE))
}

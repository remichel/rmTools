##' Creates a number of directories
##' @description \code{moveme}
##' Moves a column in a dataframe to a specified position.
##' @param invec vector of names of the dataframe of interest gathered by names(dataframe)
##' @param movecommand character string vector containing one of four movements "before", "after", "first" or "last"
##' @examples
##' # move the column to be the first column in the dataframe
##' data = data[moveme(names(data),'var first')]
##'
##' @author René Michel
##' @export moveme
##' @name moveme

moveme <- function (invec, movecommand) {
  movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]],
                                 ",|\\s+"), function(x) x[x != ""])
  movelist <- lapply(movecommand, function(x) {
    Where <- x[which(x %in% c("before", "after", "first",
                              "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  myVec <- invec
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1]])
    A <- movelist[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2]][2]
      if (A == "before") {
        after <- match(ba, temp) - 1
      }
      else if (A == "after") {
        after <- match(ba, temp)
      }
    }
    else if (A == "first") {
      after <- 0
    }
    else if (A == "last") {
      after <- length(myVec)
    }
    myVec <- append(temp, values = movelist[[i]][[1]], after = after)
  }
  myVec
}

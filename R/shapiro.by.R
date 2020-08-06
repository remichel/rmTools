##' Perform shapiro test by group
##' @description \code{shapiro.by}
##' Adopted from Marco Sandri.
##' @param data vector or dataframe with multiple columns which contain the data
##' @param group group variable which is used to split the data vector(s)
##' @examples
##' res = shapiro.by(data$reactiontime, data$group)
##' @author René Michel
##' @export shapiro.by
##' @name shapiro.by
##' @import dplyr
library(dplyr)

shapiro.by <- function(dataVars,groupVar){
  res <- lapply(dataVars, helperfun, group=groupVar)
  return(res)
}


helperfun <- function(x, group) {
  data.frame(x, group) %>%
    group_by(group) %>%
    summarise(statistic = ifelse(sd(x)!=0,shapiro.test(x)$statistic,NA),
              p.value = ifelse(sd(x)!=0,shapiro.test(x)$p.value,NA)
    )
}

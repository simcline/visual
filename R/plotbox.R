
#' Boxplot for one variable
#'
#' @param x
#'
#' @return
#' @import fsql
#' @import ggplot2
#' @import magrittr
#' @export
#'
#' @examples
#' library(fsql)
#' rnorm(100) %>% plotbox
plotbox <- function(x){
  data.frame(value = x) %>% ggplot(aes(y=value)) + geom_boxplot()
}

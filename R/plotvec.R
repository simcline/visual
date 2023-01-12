#' quick plot of a vector
#'
#' @param v vector
#'
#' @return the associated plot
#' @import fsql
#' @import ggplot2
#' @import magrittr
#' @export
#'
#' @examples
#'
#' library(fsql)
#' 1:100 %.>% sqrt %>% plotvec
plotvec <- function(v){
  v_str <- unparse(substitute(v))
  d <- data.frame(x=1:length(v))
  d[,v_str] <- v
  d[, "Series"] <- v_str
  d %>% ggplot(aes(x, .data[[v_str]])) + geom_point() + geom_line()
}

#' Quick density plot
#'
#' @param d data.frame
#' @param x variable whose density should be displayed
#' @param fill for the density curve aspect
#' @param color for the density curve aspect
#' @param alpha for the density curve aspect
#'
#' @return
#' @import fsql
#' @import ggplot2
#' @import magrittr
#' @export
#'
#' @examples
#' library(fsql)
#' d <- data.frame(x = rnorm(mean = 0,sd = 1,10000))
#' d %>% plotdens(x)
plotdens <- function(d,x, fill = "#69b3a2" , color = "#e9ecef", alpha = 0.8){
  x_str <- unparse(substitute(x))
  d %>% ggplot(aes(.data[[x_str]])) + geom_density(fill=fill, color=color, alpha=alpha)
}

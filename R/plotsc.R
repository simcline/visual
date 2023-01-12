#' quick scatter plot
#'
#' @param d
#' @param x
#' @param y
#'
#' @return the scatter ggplot
#' @export
#' @import fsql
#' @import magrittr
#' @import ggplot2
#' @examples
#' library(fsql)
#' d <- data.frame(x = c(1,2,3), y = c(5,4,6))
#'d %>% plotsc(x,y)
plotsc <- function(d, x, y){
  x_str <- unparse(substitute(x))
  y_str <- unparse(substitute(y))
  d %>% ggplot(aes(.data[[x_str]],  .data[[y_str]])) + geom_point()
}

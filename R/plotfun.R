#' function plot
#'
#' @param f one-argument function
#' @param minval xmin
#' @param maxval xmax
#' @param n steps number
#'
#' @return graph of f
#' @import fsql
#' @import ggplot2
#' @import magrittr
#' @export
#'
#' @examples
#' plotfun(\(x) sin(x^2), 0,6)
plotfun <- function(f,minval, maxval, n=1000){
  grid <- seq(minval, maxval, 1/n)
  data.frame(x=grid, y = grid %.>% f) %>% plotts(y,x=x)
}

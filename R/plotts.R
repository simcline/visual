#' Plot several columns as functions of the column x
#'
#' @param d data.frame
#' @param ... the y-axis columns
#' @param x the x-axis column
#' @param freq frequency for resampling (see help for sampleby for details)
#' @param start start time for resampling
#' @param agg aggregate function for resampling
#' @param multi multiplot? boolean
#' @param layout ignored if multi=False. Vector indicating where goes what. Example: c(1,3,2) means three subcharts, the first one hwith the first curve, second one with the next three curves and the last one with the last two curves
#'
#' @return ggplot object if multi=F, nothing otherwise
#' @import fsql
#' @import ggplot2
#' @import magrittr
#' @export
#'
#' @examples
#'
#' library(fsql)
#' d <- data.frame(
#'date=seq(as.Date("2001-01-01"), as.Date("2001-10-01"), by="month"),
#'ticker=sample(c("1 HK", "2 HK"), 10, TRUE),
#'ret1=rnorm(10),
#'ret2=rnorm(10),
#'ret3=rnorm(10)
#') %>%
#'update(price1 = cumsum(ret1), price2 = cumsum(ret2), price3 = cumsum(ret3))
#'
#'
#'d %>% plotts(price1,price2,x=date)
#'
#'d %>% plotts(price1,price2,x=date, freq="2M", agg = last)
#'
#'d %>% plotts(price1, price2, price3, x=date, multi=T)
#'
#'d %>% plotts(price1, price2, price3, x=date, multi=T, layout=c(2,1))
plotts <- function(d,...,x, freq = NULL, start = NULL, agg = function(u) mean(u,na.rm=T), multi = F, layout = NULL){
  what_str <- as.list(substitute(...())) %>% each(unparse) %>% unlist
  fun_str <- unparse(substitute(agg))
  x_str <- unparse(substitute(x))


  if (is.null(layout)) {
    layout <- rep(1, length(what_str))
  }

  if (!multi) {
    layout <- length(what_str)
  }

  env <- parent.frame()

  r <- eval(substitute(do.call(sampleby,
                              c(list(d),
                                eval(substitute(do.call(across, list(substitute(y), bylist = what_str)), list(y = parse_expr(fun_str)))),
                                alist(by = by2, freq=freq, start=start))),
                              list(by2=substitute(x), env=env)),
         enclos = env)

  list_what_str <- 1:length(layout) %>% each(\(i) what_str[(sum(layout[1:i]) - layout[i] +1):(sum(layout[1:i]))])

  v <- r %>% gather(..., key="Series",value="Value")


  if (multi) {
    list_what_str %>% each(\(w) v %>% filter(Series %in% w) %>%
                             ggplot(aes(.data[[x_str]], Value, group=Series, col = Series)) + geom_point() + geom_line()) %>%
      multiplot
  } else {
    v %>% ggplot(aes(.data[[x_str]], Value, group=Series, col = Series)) + geom_point() + geom_line()
  }
}


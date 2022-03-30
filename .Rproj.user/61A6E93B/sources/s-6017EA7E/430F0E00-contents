#' Geometric Mean
#'
#' @param x raw numeric values
#' @param na.rm should NA values be removed? (default: TRUE)
#'
#' @return a numeric vector containing transformed values
#' @export
#'
#' @examples
#'
#' gm_mean(gss_cat$age)
#'
gm_mean <- function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

#' Create Sample conditional means and standard deviation
#'
#' @param x the numeric/integer/double vector of values
#' @param g the factor or character vector to group values by
#' @param na.rm should
#'
#' @return a matrix object, containing median and IQR on two columns, and group names in rows
#' @export
#'
#' @examples
#'
#' data("gss_cat")
#' age_by_marital_status <- samp_cond_medians_IQR(gss_cat$age, g = gss_cat$marital)
#' age_by_marital_status
#'
samp_cond_medians_IQR <- function(x, g, na.rm = TRUE){
  x <- x[!is.na(g)]
  g <- g[!is.na(g)]
  median <- sapply(unique(g), function(i) median(x[grepl(i,g)], na.rm = TRUE))
  iqr <- sapply(unique(g), function(i) IQR(x[grepl(i,g)], na.rm = TRUE))
  names(median) <- unique(g)
  return(cbind(median, iqr))
}

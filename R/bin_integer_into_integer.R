
#' Bin an integer/numeric variable
#'
#' Numeric binning means that any value even closer to its next multiple, is still binned to its lowest. Example: with "multiple = 5", 4.9 is still inside the 5 bin, but 5.1 is in the 10 bin. this is not a good approximation, but sometimes you need such an approximation
#'
#' @param A `numeric` vector
#' @param multiples An `integer` or `numeric` vector to bin `x` with
#' @param include_highest Do you want to include upper or lower values in the bin? eg: if `multiples = 5` include_highest --> 'value <= 5', otherwise 'value < 5'
#'
#' @return
#' @export
#'
#' @examples
#' data(mtcars)
#' bin_integer_into_integer(mtcars$mpg, multiples = 10, include_highest= TRUE)
#' bin_integer_into_integer(mtcars$mpg, multiples = 5, include_highest= FALSE)
#'
bin_integer_into_integer <- function(x, multiples, include.lowest = FALSE){
  brks <- seq(0, max(x)+multiples, multiples)
  x_fact <- as.character(cut(x, breaks = brks,include.lowest = include.lowest)) %>%
    strsplit(split = "\\(|\\]") %>% sapply("[", 2) %>%
    strsplit(split = ",", fixed = TRUE) %>% sapply("[", 2) %>%
    as.numeric()

  return(x_fact)
}

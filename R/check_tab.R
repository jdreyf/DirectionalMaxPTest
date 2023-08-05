#' Check a matrix or data frame
#'
#' Check that an object is a matrix or data frame and ensure it contains the specified columns and that they are numeric.
#' If so, it returns `TRUE`. If not, it triggers an error. 
#'
#' @param tab Matrix-like object with statistical and p-value columns. Only the signs of the statistics columns are used.
#' `tab` should have non-duplicated row names and should not have missing values.
#' @param num.cols Vector of column indices or names that should be numeric.
#' @export

check_tab <- function(tab, num.cols){
  stopifnot(nrow(tab) > 0, ncol(tab) >= length(num.cols), num.cols %in% c(1:ncol(tab), colnames(tab)),
            is.matrix(tab) || is.data.frame(tab))
  if (is.matrix(tab)) stopifnot(is.numeric(tab[, num.cols]))
  # tests for df with list column
  # https://stackoverflow.com/questions/9547518/create-a-data-frame-where-a-column-is-a-list
  if (is.data.frame(tab)){
    stopifnot(unlist(lapply(tab[, num.cols], FUN=is.numeric)), !sapply(tab, is.list))
  }
  return(TRUE)
}
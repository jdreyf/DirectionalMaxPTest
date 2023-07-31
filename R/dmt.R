#' Directional MaxP Test
#'
#' Directional MaxP Test for two studies with signed statistics and p-values supplied via `tab`. This can be used to find rows where 
#' there is replication in a common direction or where there is mediation.
#'
#' @param tab Matrix-like object with statistical and p-value columns. Only the signs of the statistics columns are used.
#' `tab` should have non-duplicated row names and should not have missing values.
#' @param cols Vector of column indices or names in the order of `c(stat1, p1, stat2, p2)`.
#' @param prod.sgn Either `1` or `-1`. The sign of the product of the statistics under the alternative hypothesis. For replication, this should be `1`.
#' @param prefix Character string to add to beginning of column names. \code{NULL} does not add a prefix.
#' @param reorder.rows Logical, should rows be reordered by p-value?
#' @param fdr.method Character string; either "BH" for Benjamini-Hochberg or "BY" for Benjamini-Yekutieli.
#' See stats::p.adjust.
#' @return Data frame whose rows correspond to the rows of `tab` with the same row names and whose columns are
#' \describe{
#' \item{chisq}{Chi-square on 1 degreee of freedom.}
#' \item{p}{P-value}
#' \item{FDR}{FDR}
#' }
#' @details Larger chi-square values are more significant.
#' @export

dmt <- function(tab, cols=1:4, prod.sgn=1, reorder.rows=FALSE, fdr.method=c("BH", "BY"), prefix=NULL){
  fdr.method <- match.arg(fdr.method, c("BH", "BY"))
  stopifnot(nrow(tab) > 0, cols %in% c(1:ncol(tab), colnames(tab)), length(cols)==4, limma::isNumeric(tab[, cols]), prod.sgn %in% c(-1, 1))
  stat.cols <- cols[c(1, 3)]
  p.cols <- cols[c(2, 4)]
  # require rownames for consistency with hitman2_replication, which needs them st can reorder
  stopifnot(0 <= tab[, p.cols], tab[, p.cols] <= 1, !is.na(tab), is.logical(reorder.rows), !is.null(rownames(tab)))
  
  if (nrow(tab) > 10 & (all(tab[, stat.cols] >= 0) | all(tab[, stat.cols] <= 0))){
    warning("All stats are the same sign, which is possible but unlikely for more than ten two-sided stats.")
  }
  
  res <- matrix(NA, nrow=nrow(tab), ncol=3, dimnames=list(rownames(tab), c("chisq", "p", "FDR")))
  sgn <- apply(tab[, stat.cols], MARGIN=1, FUN=function(vv) sign(prod(vv)))
  # order columns per row
  p.tab.o <- t(apply(data.matrix(tab[, p.cols]), MARGIN = 1, FUN=sort, na.last=TRUE))
  colnames(p.tab.o) <- c("minp", "maxp")
  
  sgn.bool <- sgn == prod.sgn
  if (any(!sgn.bool)) res[which(!sgn.bool), "p"] <- 1
  if (any(sgn.bool)) res[which(sgn.bool), "p"] <- 0.5*p.tab.o[which(sgn.bool), "maxp"]
  
  res[, "chisq"] <- stats::qchisq(p=res[, "p"], df=1, lower.tail = FALSE)
  res[, "FDR"] <- stats::p.adjust(res[, "p"], method = fdr.method)
  
  if (reorder.rows) res <- res[order(res[, "p"]),]
  if (!is.null(prefix)) colnames(res) <- paste(prefix, colnames(res), sep=".")
  data.frame(res)
}

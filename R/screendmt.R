#' Directional MaxP Test that uses screening to improve adjusted p-values
#'
#' Directional MaxP Test hat uses screening to improve adjusted p-values for two studies with signed statistics and p-values supplied via `tab`. 
#' This can be used to find rows where there is replication in a common direction or where there is mediation.
#' 
#' @param p.adj.rate Either "FDR" for false discovery rate or "FWER" for family-wise error rate, the rate controlled by the Bonferroni procedure.
#' @inheritParams dmt
#' @return Data frame whose rows correspond to the rows of `tab` with the same row names and whose columns are
#' \describe{
#' \item{chisq}{Chi-square on 1 degree of freedom.}
#' \item{p}{P-value}
#' \item{FDR or FWER}{FDR or FWER}
#' }
#' @details Larger chi-square values are more significant.
#' @md

screendmt <- function(tab, cols=1:4, prod.sgn=1, reorder.rows=FALSE, p.adj.rate=c("FDR", "FWER"), prefix=NULL){
  p.adj.rate <- match.arg(p.adj.rate, c("FDR", "FWER"))
  stopifnot(nrow(tab) > 0, cols %in% c(1:ncol(tab), colnames(tab)), length(cols)==4, limma::isNumeric(tab[, cols]), prod.sgn %in% c(-1, 1))
  stat.cols <- cols[c(1, 3)]
  p.cols <- cols[c(2, 4)]
  # don't reorder rows if don't have row names to identify the rows
  stopifnot(0 <= tab[, p.cols], tab[, p.cols] <= 1, !is.na(tab), is.logical(reorder.rows), !is.na(prefix), !is.null(rownames(tab)))
  
  hm <- dmt(tab=tab, cols=cols, prod.sgn = prod.sgn, reorder.rows=FALSE, fdr.method="BH", prefix=NULL) |>
    dplyr::select(!FDR)
  M <- nrow(tab)
  
  tab2 <- data.frame(tab[, p.cols], hm) |>
    dplyr::mutate(minp = apply(as.matrix(tab[, p.cols]), MARGIN=1, FUN=min), max2 = pmax(p, minp)) |>
    dplyr::arrange(max2) |>
    dplyr::mutate(rnk = 1:M)
  tab2$adj.num <- apply(as.matrix(tab2$max2), 1, FUN=function(xx) sum(tab2$minp <= xx))
  
  if (p.adj.rate == "FDR"){
    tab2 <- tab2 |> dplyr::mutate(bh.point = adj.num*max2/rnk, FDR = cummin(bh.point[M:1])[M:1]) |>
      dplyr::select(!(minp:bh.point))
  } else {
    tab2 <- tab2 |> dplyr::mutate(holm.point = adj.num*max2, FWER = pmin(cummin(holm.point[M:1])[M:1], 1)) |>
      dplyr::select(!(minp:holm.point))
  }
  
  if (reorder.rows){
    tab2 <- tab2 |> dplyr::arrange("p")
  } else {
    tab2 <- tab2[rownames(tab),]
  }
  if (!is.null(prefix)) colnames(tab2) <- paste(prefix, colnames(tab2), sep=".")
  tab2 |> dplyr::select(-(1:2))
}

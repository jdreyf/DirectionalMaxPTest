#' Directional MaxP Test that uses screening to improve adjusted p-values
#'
#' Directional MaxP Test that uses screening to improve adjusted p-values for two studies with signed statistics and p-values supplied via `tab`. 
#' This can be used to find rows where there is replication in a common direction or where there is mediation.
#' 
#' @param p.adj.rate Either "FDR" for false discovery rate or "FWER" for family-wise error rate, the rate controlled by the Bonferroni procedure.
#' @param keep.input Logical, should returned data frame include `tab[, cols]`? If so, `tab` must have column names and these
#' cannot include "chisq", "p", and "FDR" or "FWER" (depending on `p.adj.rate`).
#' @inheritParams dmt
#' @return Data frame whose rows correspond to the rows of `tab` with the same row names and whose columns include
#' \describe{
#' \item{chisq}{Chi-square on 1 degree of freedom}
#' \item{p}{P-value}
#' \item{FDR or FWER}{Adjusted p-value}
#' }
#' @details Larger chi-square values are more significant. `tab` must have more than one row.
#' @md
#' @export

screendmt <- function(tab, cols=1:4, prod.sgn=1, reorder.rows=FALSE, p.adj.rate=c("FDR", "FWER"), prefix=NULL, keep.input=FALSE){
  p.adj.rate <- match.arg(p.adj.rate, c("FDR", "FWER"))
  stopifnot(length(cols)==4, prod.sgn %in% c(-1, 1))
  check_tab(tab, num.cols = cols)
  stat.cols <- cols[c(1, 3)]
  p.cols <- cols[c(2, 4)]
  # don't reorder rows if don't have row names to identify the rows
  stopifnot(0 <= tab[, p.cols], tab[, p.cols] <= 1, !is.na(tab[, cols]), is.logical(reorder.rows), is.logical(keep.input), !is.na(prefix), 
            !is.null(rownames(tab)), nrow(tab) > 1)
  if (keep.input) stopifnot(!is.null(colnames(tab)), !(colnames(tab)[cols] %in% c("chisq", "p")))
  if (keep.input & p.adj.rate=="FDR") stopifnot(!colnames(tab)[cols] %in% "FDR")
  if (keep.input & p.adj.rate=="FWER") stopifnot(!colnames(tab)[cols] %in% "FWER")
  
  # don't keep input here, since input could contain column names that clash with temporary columns
  hm <- dmt(tab=tab, cols=cols, prod.sgn = prod.sgn, reorder.rows=FALSE, fdr.method="BH", prefix=NULL) |>
    dplyr::select(!FDR)
  M <- nrow(tab)
  
  tab2 <- data.frame(tab[, p.cols, drop=FALSE], hm) |>
    dplyr::mutate(minp = apply(as.matrix(tab[, p.cols, drop=FALSE]), MARGIN=1, FUN=min), max2 = pmax(p, minp)) |>
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
  
  # first two columns are tab[, p.cols]
  tab2 <- tab2[rownames(tab),, drop=FALSE] |> dplyr::select(-(1:2))
  if (keep.input) tab2 <- data.frame(tab2, tab[, cols, drop=FALSE])
  if (reorder.rows) tab2 <- tab2 |> dplyr::arrange(dplyr::across(dplyr::matches("F(D|WE)R")), p)
  if (!is.null(prefix)) colnames(tab2)[1:3] <- paste(prefix, colnames(tab2)[1:3], sep=".")
  tab2
}
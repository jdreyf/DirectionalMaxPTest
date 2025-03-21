#' Mediation analysis using ScreenDMT with glm
#'
#' Mediation analysis for many analytes with ScreenDMT. It tests if rows of \code{M} mediate the effect of exposure \code{E} on outcome
#' \code{Y}. The associations are tested using base R's `glm` or `lm`. 
#' Before applying, you should know a priori the direction that \code{E} changes \code{Y} and verify
#' that \code{E} changes \code{Y} significantly and in the same direction here.
#'
#' @param E A numeric vector of exposures.
#' @param M A numeric matrix with one row per feature and one column per sample of mediators.
#' Must have more than one feature and have row names and column names.
#' @param Y A numeric vector of \code{length(E)} of outcomes. Only continuous, normally distributed outcomes
#' currently supported.
#' @param covariates Numeric vector with one element per sample or matrix-like object with rows corresponding
#' to samples and columns to covariates to be adjusted for.
#' @param fam Character string of family to use in generalized linear model of \code{Y}. The default
#' \code{"gaussian"} reduces to the usual linear regression model. See stats::family.
#' @param verbose Logical; should messages be given for lack of association between \code{E} & \code{Y} and filtering?
#' @param check.names Logical; should \code{names(E)==colnames(M) & colnames(M)==names(Y)} be checked?
#' @inheritParams dmt
#' @inheritParams screendmt
#' @return Data frame with columns
#' \describe{
#' \item{EMY.chisq}{Overall chi-square for mediation on 1 degreee of freedom.}
#' \item{EMY.p}{Overall p-value for mediation}
#' \item{EMY.FDR}{Overall FDR for mediation}
#' \item{EM.z}{z-score for E-->M, not accounting for direction}
#' \item{EM.p}{p-value for E-->M, not accounting for direction}
#' \item{MY.z}{z-score for M-->Y, not accounting for direction}
#' \item{MY.p}{p-value for M-->Y, not accounting for direction}
#' }
#' @details \code{E} and \code{Y} cannot have \code{NA}s. \code{M} may have some \code{NA}s.
#'
#' Larger chi-square values are more significant.
#' @export

mediate_glm <- function(E, M, Y, covariates=NULL, fam="gaussian", reorder.rows=TRUE, p.adj.rate=c("FDR", "FWER"),
                   verbose=TRUE, check.names=TRUE){
  p.adj.rate <- match.arg(p.adj.rate, c("FDR", "FWER"))
  check_tab(M, num.cols = ncol(M))
  stopifnot(is.numeric(E), is.numeric(Y), !is.na(E), !is.na(Y), is.null(dim(E)), is.null(dim(Y)), stats::var(E) > 0,
            nrow(M) > 1, length(E)==ncol(M), length(Y)==ncol(M), !is.null(rownames(M)), !is.null(colnames(M)),
            length(unique(Y)) >= 3 || fam == "binomial")
  if (check.names) stopifnot(names(E)==colnames(M), colnames(M)==names(Y))
  
  # ok if covariates is NULL
  my.covar <- cbind(E=E, covariates=covariates)
  
  # test EY; return ey.sign & weak assoc warning
  if (fam == "gaussian"){
    fm.ey <- stats::lm(Y ~ ., data=data.frame(Y, my.covar))
    tt.ey <- c(EY.t=summary(fm.ey)$coefficients["E", "t value"], EY.p=summary(fm.ey)$coefficients["E", "Pr(>|t|)"])
  } else {
    fm.ey <- stats::glm(Y ~ ., data=data.frame(Y, my.covar), family=fam)
    tt.ey <- c(EY.t=summary(fm.ey)$coefficients["E", "z value"], EY.p=summary(fm.ey)$coefficients["E", "Pr(>|z|)"])
  }
  
  if (tt.ey["EY.p"] > 0.05 && verbose){
    message("E and Y are not associated at p<0.05 (p=", signif(tt.ey["EY.p"], digits = 2),
            "), so mediation may not be meaningful.")
  }
  ey.sign <- sign(tt.ey["EY.t"])
  
  tt <- matrix(NA, nrow=nrow(M), ncol=4, dimnames=list(rownames(M), c("EM.t", "EM.p", "MY.t", "MY.p")))
  for (rr in 1:nrow(M)){
    m.v <- M[rr,]
    fm.em <- stats::lm(m.v ~ ., data = data.frame(my.covar))
    tt[rr, c("EM.t", "EM.p")] <- summary(fm.em)$coefficients["E", c("t value", "Pr(>|t|)")]
    fm.my <- stats::lm(Y ~ m.v + ., data = data.frame(my.covar))
    tt[rr, c("MY.t", "MY.p")] <- summary(fm.my)$coefficients["m.v", c("t value", "Pr(>|t|)")]
  }
  
  ret <- screendmt(tab=tt, prod.sgn = ey.sign, reorder.rows = FALSE, p.adj.rate = p.adj.rate, keep.input = TRUE, prefix = "EMY")

  if (reorder.rows) ret <- ret[order(ret$EMY.p),]
  return(ret)
}
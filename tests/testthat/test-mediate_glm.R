test_that("mediate_glm runs without errors on basic data", {
  result <- mediate_glm(E = E, M = M, Y = Y, reorder.rows = FALSE)
  expect_s3_class(result, "data.frame")
})

test_that("mediate_glm without covariates", {
  result <- mediate_glm(E = E, M = M, Y = Y)
  expected_cols <- c("EMY.chisq", "EMY.p", "EMY.FDR", "EM.t", "EM.p", "MY.t", "MY.p")
  expect_true(all(expected_cols %in% colnames(result)))
  expect_lte(result["G1", "EMY.p"], 0.1)
  
  em.fm1 <- stats::lm(M[1,] ~ E)
  expect_equal(result[1, "EM.t"], summary(em.fm1)$coefficients["E", "t value"])
  my.fm2 <- stats::lm(Y ~ M[2,] + E)
  expect_equal(result[2, "MY.t"], summary(my.fm2)$coefficients[2, "t value"])
})

test_that("mediate_glm handles covariates", {
  covariate <- rnorm(length(E))
  result <- mediate_glm(E = E, M = M, Y = Y, covariates = covariate, reorder.rows = FALSE)
  em.fm1 <- stats::lm(M[1,] ~ E + covariate)
  expect_equal(result[1, "EM.t"], summary(em.fm1)$coefficients["E", "t value"])
  my.fm2 <- stats::lm(Y ~ M[2,] + E + covariate)
  expect_equal(result[2, "MY.t"], summary(my.fm2)$coefficients[2, "t value"])
  
  covariates_matrix <- matrix(rnorm(2 * length(E)), ncol = 2)
  result2 <- mediate_glm(E = E, M = M, Y = Y, covariates = covariates_matrix, reorder.rows = FALSE)
  em.fm1 <- stats::lm(M[1,] ~ E + covariates_matrix)
  expect_equal(result2[1, "EM.t"], summary(em.fm1)$coefficients["E", "t value"])
  my.fm2 <- stats::lm(Y ~ M[2,] + E + covariates_matrix)
  expect_equal(result2[2, "MY.t"], summary(my.fm2)$coefficients[2, "t value"])
  
  res3 <- mediate_glm(E = E, M = M, Y = Y, covariates = cvrts.mod, reorder.rows = FALSE)
  em.fm1 <- stats::lm(M[1,] ~ 1 + E + x1 + lvllow + lvlmed, data=as.data.frame(cvrts.mod))
  expect_equal(res3[1, "EM.t"], summary(em.fm1)$coefficients["E", "t value"])
  my.fm2 <- stats::lm(Y ~ M[2,] + E + x1 + lvllow + lvlmed, data=as.data.frame(cvrts.mod))
  expect_equal(res3[2, "MY.t"], summary(my.fm2)$coefficients[2, "t value"])
})

test_that("mediate_glm handles weak EY association warning", {
  set.seed(0)
  E <- rnorm(100)
  M1 <- 0.5 * E + rnorm(100)
  M2 <- 0.5 * E + rnorm(100)
  M <- rbind(M1,M2)
  colnames(M) <- paste0("M", 1:ncol(M))
  rownames(M) <- paste0("Sample", 1:nrow(M))
  Y <- rnorm(100) # No association with E
  expect_message(mediate_glm(E = E, M = M, Y = Y))
})

test_that("mediate_glm handles invalid input", {
  E2 <- E
  E2[1] <- NA
  expect_error(mediate_glm(E = E2, M = M, Y = Y))
  
  Y2 <- Y
  Y2[1] <- NA
  expect_error(mediate_glm(E = E, M = M, Y = Y2))
  
  expect_error(mediate_glm(E = E, M = matrix(NA, nrow=0, ncol=100), Y = Y))
  
  expect_error(mediate_glm(E = E[1:99], M = M, Y = Y))
  expect_error(mediate_glm(E = E, M = M, Y = Y[1:99]))
  
  expect_error(mediate_glm(E = as.character(E), M = M, Y = Y))
  expect_error(mediate_glm(E = E, M = M, Y = as.character(Y)))
  
  E2 <- rep(1,100)
  expect_error(mediate_glm(E = E2, M = M, Y = Y))
})
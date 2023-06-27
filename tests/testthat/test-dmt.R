test_that("correct significances", {
  set.seed(0)
  tab.tmp <- data.frame(matrix(NA, nrow=100, ncol=4, dimnames=list(paste0("r", 1:100), c("stat1", "p1", "stat2", "p2"))))
  tab.tmp[, c(1, 3)] <- rnorm(n=200)
  tab.tmp[, 2] <- 2*pnorm(-abs(tab.tmp[, 1]))
  tab.tmp[, 4] <- 2*pnorm(-abs(tab.tmp[, 3]))
  
  # significances
  hmr <- dmt(tab=tab.tmp)
  expect_equal(hmr["r10", grep("p$", colnames(hmr))], 0.5*max(tab.tmp["r10", c(2, 4)]))
  # r3 is inconsistent
  expect_equal(hmr["r3", grep("p$", colnames(hmr))], 1)
  
  hmby <- dmt(tab=tab.tmp, fdr.method = "BY")
  expect_true(all(hmby$FDR >= hmr$FDR))
  
  hmr2 <- dmt(tab=tab.tmp, prod.sgn = -1)
  expect_true(all(hmr2[hmr$NA.p == 1, "p"] < 1))
  
  # one row only
  expect_silent(dmt1 <- dmt(tab=tab.tmp[1,]))
  
  # invalid input
  expect_error(tab.tmp |> dplyr::mutate(stat1 = as.character(stat1)) |>
                 dmt())
  expect_error(tab.tmp |> dmt(cols=2:5))
  expect_error(tab.tmp |> dmt(cols=colnames(tab.tmp)[-1]))
  
  tab.na <- tab.tmp
  tab.na[1,1] <- NA
  expect_error(tab.na |> dmt())
  
  # only matrices can have NULL row names
  tab2 <- tab.tmp |> as.matrix()
  rownames(tab2) <- NULL
  expect_error(tab2 |> dmt(reorder.rows = TRUE))
})

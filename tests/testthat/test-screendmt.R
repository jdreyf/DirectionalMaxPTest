test_that("correct significances", {
  hmr <- dmt(tab=tab.tmp, reorder.rows = FALSE, fdr.method = "BH")
  hm2r <- screendmt(tab=tab.tmp, reorder.rows = FALSE, p.adj.rate = "FDR", keep.input = TRUE)
  hm2r.fwer <- screendmt(tab=tab.tmp, reorder.rows = FALSE, p.adj.rate = "FWER")
  expect_true(all(rownames(hm2r) == rownames(tab.tmp)))
  expect_equal(ncol(hm2r), 7)
  expect_true(all.equal(hm2r[, 1:2], hmr[, 1:2]))
  expect_true(all.equal(hm2r.fwer[, 1:2], hmr[, 1:2]))
  # r10 has lowest p & FDR
  expect_lt(hm2r[10, "FDR"], hmr[10, "FDR"])
  expect_true(all(hm2r$FDR <= hm2r.fwer$FWER))
  
  expect_error(screendmt(tab=tab.tmp[1,, drop=FALSE], reorder.rows = FALSE, fdr.method = "BH"))
  # keep.input with clashing column names
  tab2.tmp <- tab.tmp |> dplyr::rename(p = p1)
  expect_error(screendmt(tab2.tmp, fdr.method = "BY", keep.input=TRUE))
})

# test_that("matches hitman2_replication", {
#   sdmtr <- screendmt(tab=tab.tmp)
#   hm2r <- Hitman:::hitman2_replication(tab=tab.tmp)
#   expect_true(all(sdmtr == hm2r))
# })



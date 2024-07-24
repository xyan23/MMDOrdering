test_that("heapsort_c() Order points using heap structure in Rcpp", {
  m<-matrix(c(c(2, 9, 7, 6, 5, 8), c(1, 2, 3, 4, 5, 6)), nrow = 6, ncol = 2)

  expect_equal(unlist(as.list(heapsort_c(m))),
               c(2, 5, 6, 7, 8, 9, 1, 5, 4, 3, 6, 2))
})

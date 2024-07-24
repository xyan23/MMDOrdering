test_that("heapsort() Order points using heap structure in R", {
  m<-matrix(c(c(2, 9, 7, 6, 5, 8), c(1, 2, 3, 4, 5, 6)), nrow = 6, ncol = 2)
  compare_function1 <- function(p1, p2) {
    comparison_criterion <- p1[1] > p2[1]}

  expect_equal(unlist(as.list(heapsort(m,compare_function1))),
               c(2, 5, 6, 7, 8, 9, 1, 5, 4, 3, 6, 2))
})

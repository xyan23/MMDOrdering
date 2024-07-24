test_that("heapsort() Test the heapsort function in R and cpp file have the same result", {
  m<-matrix(c(c(2, 9, 7, 6, 5, 8), c(1, 2, 3, 4, 5, 6)), nrow = 6, ncol = 2)
  compare_function1 <- function(p1, p2) {
    comparison_criterion <- p1[1] > p2[1]}

  expect_equal(heapsort(m,compare_function1),
               heapsort_c(m))
})

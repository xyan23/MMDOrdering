test_that("heap_mmd() order points using mmd", {
  m <- matrix(c(c(1, 3, 0, 2, 2), c(5, 3, 1, 2, 0)), nrow = 5, ncol = 2)
  compare_mmd <- function(p1, p2) {
    comparison_criterion <- p1[3] > p2[3]}

  expect_equal(unlist(as.list(heap_mmd(m,compare_mmd))),
            c(2, 1, 0, 2, 3, 2, 5, 1, 0, 3))
})

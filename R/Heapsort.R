#' Check point on each index to fit heap property
#'
#' @param Matrix A n by 2 matrix where each row represents a point.
#' @param index Point index.
#' @param comp_fun A compare function which states compare criterion in heap.
#'
#' @return A matrix of points.
#' @export
#'
#' @examples
#' x<-c(2, 9, 7, 6, 5, 8)
#' y<-c(1, 2, 3, 4, 5, 6)
#' m<-matrix(c(x, y), nrow = length(x), ncol = 2)
#' compare_function1 <- function(p1, p2) {
#'   comparison_criterion <- p1[1] > p2[1]
#' }
#' heapify(m, 2, compare_function1)

heapify <- function(Matrix, index, comp_fun) {
  heap_size <- nrow(Matrix)
  # Left child index
  left <- 2 * index
  # Right child index
  right <- 2 * index + 1
  # Make sure this is a complete binary tree
  # Compare between root and left child
  if (left <= heap_size && comp_fun(Matrix[left, ], Matrix[index, ])) {
    largest <- left
  } else {
    largest <- index
  }
  # Compare between largest and left child
  if (right <= heap_size && comp_fun(Matrix[right, ], Matrix[largest, ])) {
    largest <- right
  }
  # If largest is not root
  if (largest != index) {
    # Swap the larger child with parent
    key <- Matrix[index, ]
    Matrix[index, ] <- Matrix[largest, ]
    Matrix[largest, ] <- key
    # Recursively heapify for sub-tree
    Matrix <- heapify(Matrix, largest, comp_fun)
  }

  return(Matrix)
}

#' Build heap matrix
#'
#' @param Matrix A n by 2 matrix where each row represents a point.
#' @param comp_fun A compare function which states compare criterion in heap.
#'
#' @return A matrix of points with heap property.
#' @export
#'
#' @examples
#' x<-c(2, 9, 7, 6, 5, 8)
#' y<-c(1, 2, 3, 4, 5, 6)
#' m<-matrix(c(x, y), nrow = length(x), ncol = 2)
#' compare_function1 <- function(p1, p2) {
#'   comparison_criterion <- p1[1] > p2[1]
#' }
#' build_heap(m, compare_function1)

build_heap <- function(Matrix, comp_fun) {
  n <- nrow(Matrix)
  # Check all the parental nodes
  for (i in floor(n / 2):1) {
    Matrix <- heapify(Matrix, i, comp_fun)
  }
  return(Matrix)
}

#' Using heap with certain compare criterion to order points.
#'
#' @param Matrix  A n by 2 matrix where each row represents a point.
#' @param comp_fun A compare function which states compare criterion in heap.
#'
#' @return A matrix of ordered points.
#' @export
#'
#' @examples
#' x<-c(2, 9, 7, 6, 5, 8)
#' y<-c(1, 2, 3, 4, 5, 6)
#' m<-matrix(c(x, y), nrow = length(x), ncol = 2)
#' compare_function1 <- function(p1, p2) {
#'   comparison_criterion <- p1[1] > p2[1]
#' }
#' heapsort(m, compare_function1)

heapsort <- function(Matrix, comp_fun) {
  n <- nrow(Matrix)
  Matrix <- build_heap(Matrix, comp_fun)
  for (i in n:2) {
    # Exchange the first element with the ith element
    key <- Matrix[1, ]
    Matrix[1, ] <- Matrix[i, ]
    Matrix[i, ] <- key
    # Decrease the heap size by 1
    Matrix[1:(i-1), ] <- heapify(Matrix[1:(i-1), , drop = FALSE], 1, comp_fun)
  }
  return(Matrix)
}

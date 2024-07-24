#' Use maximum minimum distance to order points
#'
#' @param Matrix A n by 2 matrix where each row represents a point.
#' @param comp_fun A compare function which states compare criterion in heap.
#'
#' @return A matrix of ordered points.
#' @export
#'
#' @examples
#' x <- c(1, 3, 0, 2, 2)
#' y <- c(5, 3, 1, 2, 0)
#' m <- matrix(c(x, y), nrow = length(x), ncol = 2)
#' compare_mmd <- function(p1, p2) {
#' comparison_criterion <- p1[3] > p2[3]}
#' heap_mmd(m, compare_mmd)

heap_mmd <- function(Matrix, comp_fun) {
  #Calculate the distance between two points
  distance <- function(point1, point2) {
    return(sqrt(sum((point1 - point2)^2)))
  }

  # Start with a random point
  start_index <- sample(1:nrow(Matrix), 1)
  # Store ordered points
  ordered_points <- Matrix[start_index, , drop = FALSE]
  # Store remaining points
  remaining_points <- Matrix[-start_index, , drop = FALSE]

  # Find the distance from the first ordered point
  dist_vec <- c()
  for (i in 1:nrow(remaining_points)) {
    dist_vec[i] <- distance(remaining_points[i, ], ordered_points[1, ])
  }
  # Add minimum distance as the third column of the remaining points matrix
  remaining_points <- cbind(remaining_points, dist_vec)
  # Find the maximum minimum distance
  remaining_points <- build_heap(remaining_points, comp_fun)
  ordered_points <- rbind(ordered_points, remaining_points[1, -3])
  remaining_points <- remaining_points[-1, , drop = FALSE]

  while (nrow(remaining_points) > 1){
    # Find the maximum minimum distance for the rest of points
    for (i in 1:nrow(remaining_points)) {
      num_ordered_points <- nrow(ordered_points)
      dist <- distance(remaining_points[i,-3], ordered_points[num_ordered_points, ])
      if (dist < remaining_points[i,3]) {
        remaining_points[i,3] <- dist
      }
    }
    remaining_points <- build_heap(remaining_points, comp_fun)
    ordered_points <- rbind(ordered_points, remaining_points[1, -3])
    remaining_points <- remaining_points[-1, , drop = FALSE]
  }
  ordered_points <- rbind(ordered_points, remaining_points[1, -3])
  return(ordered_points)
}

# Compare criterion for heap, which compares the third column(minimum distance to previous ordered points)
# between two rows (represents two points)
compare_mmd <- function(p1, p2) {
  comparison_criterion <- p1[3] > p2[3]
}

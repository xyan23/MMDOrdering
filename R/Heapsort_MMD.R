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
#' heap_mmd_ver2(m, compare_mmd)

max_heapify <- function(Matrix, index, comp_fun) {
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
    Matrix <- max_heapify(Matrix, largest, comp_fun)
  }

  return(Matrix)
}

build_max_heap <- function(Matrix, comp_fun) {
  n <- nrow(Matrix)
  # Check all the parental nodes
  for (i in floor(n / 2):1) {
    Matrix <- max_heapify(Matrix, i, comp_fun)
  }
  return(Matrix)
}

#Calculate the distance
distance <- function(point1, point2) {
  return(sqrt(sum((point1 - point2)^2)))
}

#Calculate the minimum distance
min_distance <- function(point, ordered_points){
  min(sapply(ordered_points, function(point) {
    distance(point, point)
  }))
}

heap_mmd_ver1 <- function(Matrix, comp_fun) {
  # Start with a random point
  start_index <- sample(1:nrow(Matrix), 1)
  # Store ordered points
  ordered_points <- Matrix[start_index, , drop = FALSE]
  # Store remaining points
  remaining_points <- Matrix[-start_index, , drop = FALSE]

  while (nrow(remaining_points) > 1){
    # Find minimum distance
    min_dist_vec <- c()
    for (i in 1:nrow(remaining_points)) {
      dist_vec <- c()
      for (j in 1:nrow(ordered_points)){
        dist_vec[j] <- distance(remaining_points[i, ], ordered_points[j, ])
      }
      min_dist_vec[i] <- min(dist_vec)
    }

    remaining_points <- cbind(remaining_points, min_dist_vec)

    remaining_points <- build_max_heap(remaining_points, comp_fun)

    ordered_points <- rbind(ordered_points, remaining_points[1, -3])

    remaining_points <- remaining_points[-1, -3, drop = FALSE]
  }
  ordered_points <- rbind(ordered_points, remaining_points[1, -3])
  return(ordered_points)
}

heap_mmd_ver2 <- function(Matrix, comp_fun) {
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
  remaining_points <- build_max_heap(remaining_points, comp_fun)
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
    remaining_points <- build_max_heap(remaining_points, comp_fun)
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

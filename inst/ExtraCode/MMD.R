#' Order points with MMD, without using heap structure (There still some bug in the code)
#'
#' @param matrix A n by 2 matrix where each row represents a point.
#'
#' @return A matrix of ordered points.
#' @export
#'
#' @examples
#' x<-c(2, 9, 3)
#' y<-c(1, 2, 1)
#' m<-matrix(c(x, y), nrow = length(x), ncol = 2)
#' mmd_order(m)

mmd_order <- function(matrix) {
  # Calculate the distance between two points
  distance <- function(p1, p2) {
    return(sqrt(sum((p1 - p2)^2)))
  }
  # Start with a random point
  start_index <- sample(1:nrow(matrix), 1)
  # Store ordered points
  ordered_points <- list(matrix[start_index, ])
  # Store remaining points
  remaining_points <- matrix[-start_index, , drop = FALSE]
  
  while (nrow(remaining_points) > 0) {
    # Calculate the minimum distance
    min_distance <- for (i in 1:nrow(remaining_points)) {
      min(sapply(ordered_points, function(point) {
        distance(remaining_points[i, ], point)
      }))
    }
    
    # Find the index of the point with maximum-minimum distance
    max_index <- which.max(min_distance) 
    # Add point to ordered points
    ordered_points <- append(ordered_points, list(remaining_points[max_index, ]))
    # Remove point from remaining points
    remaining_points <- remaining_points[-max_index, , drop = FALSE]
  }
  
  #ordered_points <- do.call(rbind, ordered_points)
  return(ordered_points)
}

print(mmd_order(m))



grid_order_points <- function(points, comp_fun, grid_x_dim, grid_y_dim) {
  # Determine the size of each grid box
  x_range <- 1
  y_range <- 1
  grid_x_size <- x_range/grid_x_dim
  grid_y_size <- y_range/grid_y_dim
  num_boxes <- grid_x_dim * grid_y_dim

  # Find the center of each grid
  center_x <- seq(from = grid_x_size/2, to = x_range, by = grid_x_size)
  center_y <- seq(from = grid_y_size/2, to = y_range, by = grid_y_size)
  grid_center <- as.matrix(expand.grid(center_x, center_y))

  # Use MMD and grid center points to order the grid boxes
  ordered_grid <- heap_mmd(grid_center, compare_mmd)
  ordered_grid <- cbind(ordered_grid,
                        floor(ordered_grid[ , 1] / grid_x_size) + 1,
                        floor(ordered_grid[ , 2] / grid_y_size) + 1,
                        c(1:num_boxes))

  # Assign the points to the grid_box
  ordered_grid_index <- rep(NA, nrow(points))
  dist_vec <- rep(NA, nrow(points))
  points <- cbind(points,
                  dist_vec,
                  floor(points[ , 1] / grid_x_size) + 1,
                  floor(points[ , 2] / grid_y_size) + 1,
                  ordered_grid_index)

  for (i in 1:nrow(points)) {
    same_grid_index <- which(points[i,3:4] == ordered_grid[ , 3:4])
    if (length(same_grid_index) > 0) {
      #Debug
      points[i,5] <- ordered_grid[same_grid_index[1],5]
    }
  }
  print(points)
}

grid_box_ordering <- function(points, comp_fun) {
  # Calculate the distance between two points
  distance <- function(point1, point2) {
    return(sqrt(sum((point1 - point2)^2)))
  }

  # Determine start point, the point closest to the first grid center
  first_dist <- rep(NA, nrow(points))

  for (i in 1:nrow(points)) {
    if (points[i,5] == 1) {
      point[i] <- distance(point[i,], ordered_grid[1, ])
    }
  }
  start_index <- which.min(first_dist)
  ordered_points <- points[start_index, , drop = FALSE]
  remaining_points <- points[-start_index, , drop = FALSE]

  # Find the distance from the first ordered point
  for (i in 1:nrow(remaining_points)) {
    points[i,3] <- distance(remaining_points[i, ], ordered_points[1, ])
  }

  sub2_remaining_points <- remaining_points[remaining_points[ ,6] == 2, ]

  sub2_remaining_points <- build_heap(sub2_remaining_points, comp_fun)
  ordered_points <- rbind(ordered_points, sub2_remaining_points[1, -3])
  sub2_remaining_points <- sub2_remaining_points[-1, , drop = FALSE]

  for (i in 1:nrow(remaining_points)) {
    remaining_index <- which(remaining_points[i, ] == sub2_remaining_points[1, ])
    remaining_points <- remaining_points[-remaining_index, , drop = FALSE]
  }

}

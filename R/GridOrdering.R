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

  # Find the location of grid, coord = 1 for x-coord, coord = 2 for y-coord
  grid_loc <- function(points, coord, grid_size){
    floor(points[ , coord] / grid_size) + 1
  }

  ordered_grid <- cbind(ordered_grid,
                        grid_loc(ordered_grid, 1, grid_x_size),
                        grid_loc(ordered_grid, 2, grid_y_size),
                        grid_order = c(1:num_boxes))
  ordered_grid <- cbind(ordered_grid,
                        # The fourth column for column and the fifth column for row
                        grid_index = max(ordered_grid[, 3]) * (ordered_grid[, 4] - 1) + ordered_grid[, 3])


  # Assign the points to the grid_box
  points <- cbind(points,
                  dist_vec = rep(NA, nrow(points)),
                  grid_loc(points, 1, grid_x_size),
                  grid_loc(points, 2, grid_y_size),
                  grid_order = rep(NA, nrow(points)))

  points <- cbind(points,
                  # The fourth column for column and the fifth column for row
                  grid_index = grid_x_dim * (points[, 5] - 1) + points[, 4],
                  index = c(1:nrow(points)))

  for (i in 1:nrow(points)) {
    same_grid_index <- which(points[i, 7] == ordered_grid[ , 6])
    points[i,6] <- ordered_grid[same_grid_index, 5]
  }
  print(points)
}

grid_box_ordering <- function(points, comp_fun) {
  # Calculate the distance between two points
  distance <- function(point1, point2) {
    return(sqrt(sum((point1 - point2)^2)))
  }

  points <- points[, -c(4:5)]
  points <- cbind(points,
                  ordered_points = rep(NA, nrow(points)))

  # Determine neighboring grid
  neighboring_grid <- function(index){
    if (index == 1) {
      neighboring_grid_index <- c(index + 1, index + grid_x_dim)
    } else if (index == grid_x_dim) {
      neighboring_grid_index <- c(index - 1, index + grid_x_dim)
    } else if (index == (grid_y_dim - 1) * grid_x_dim + 1) {
      neighboring_grid_index <- c(index + 1, index - grid_x_dim)
    } else if (index == num_boxes) {
      neighboring_grid_index <- c(index - 1, index - grid_x_dim)
    } else {
      neighboring_grid_index <- c(index + 1, index - 1, index + grid_x_dim, index - grid_x_dim)
    }
  }

  remaining_points <- points
  ordered_points <- matrix(nrow = 0, ncol = 7)
  sub_ordered_points <- matrix(nrow = 0, ncol = 7)
  # Order the remaining points
  while (nrow(remaining_points) > 1) {
    for (i in 1:num_boxes){
      # Find points in the neighborhood
      neighboring_grid_index <- neighboring_grid(i)
      sub_points <- remaining_points[remaining_points[ , 4] == i, ]
      # ???? Debug
      for (k in 1:length(neighboring_grid_index)) {
        neighbor_points <- remaining_points[remaining_points[ , 4] == neighboring_grid_index[k]]
        sub_points <- rbind(sub_points,
                            neighbor_points)
      }

      # Check is there already an ordered points
      check_column_na <- function(matrix, col_index) {
        return(all(is.na(matrix[, col_index])))
      }

      # If there is not an ordered points in the neighborhood, find the point closest to center
      if (check_column_na(sub_points, 7)) {
        first_dist <- rep(NA, nrow(sub_points))
        for (j in 1:nrow(sub_points)) {
          first_dist[j] <- distance(sub_points[j,], ordered_grid[i, ])
        }
        start_index <- which.min(first_dist)
        sub_ordered_points <- rbind(sub_ordered_points,
                                    sub_points[start_index, , drop = FALSE])
        # Find the point's original index
        point_index <- sub_points[start_index, 6]
        ordered_points <- rbind(ordered_points,
                                remaining_points[point_index, , drop = FALSE])

        sub_remaining_points <- sub_points[-start_index, , drop = FALSE]
        remaining_points <- remaining_points[-point_index, , drop = FALSE]


        # Mark points as ordered
        sub_points[start_index, 7] <- 1

      } else {
        # Current gird points
        current_grid_points <- remaining_points[remaining_points[ , 4] == i, ]
        # Find the next ordered points
        for (j in 1:nrow(current_grid_points)) {
          num_sub_ordered_points <- nrow(sub_ordered_points)
          dist <- distance(current_grid_points[j,], sub_ordered_points[num_sub_ordered_points, ])
          if (is.na(current_grid_points[j, 3])) {
            current_grid_points[j, 3] <- dist
          } else if (dist < current_grid_points[i,3]) {
            current_grid_points[j, 3] <- dist
          }
        }
        current_grid_points <- build_heap(current_grid_points, compare_mmd)
        sub_ordered_points <- rbind(sub_ordered_points,
                                    current_grid_points[1, ])
        point_index <- current_grid_points[1, 6]
        ordered_points <- rbind(ordered_points,
                                current_grid_points[1, ])

        remaining_points <- remaining_points[-point_index, , drop = FALSE]
      }
    }
  }
  ordered_points <- rbind(ordered_points, remaining_points[1, ])
  return(ordered_points)
}

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
                        col_loc = grid_loc(ordered_grid, 1, grid_x_size),
                        row_loc = grid_loc(ordered_grid, 2, grid_y_size),
                        grid_order = c(1:num_boxes))
  ordered_grid <- cbind(ordered_grid,
                        # The fourth column for column and the fifth column for row
                        grid_index = grid_x_dim * (ordered_grid[, "row_loc"] - 1) + ordered_grid[, "col_loc"])


  # Assign the points to the grid_box
  points <- cbind(points,
                  dist_vec = rep(NA, nrow(points)),
                  col_loc = grid_loc(points, 1, grid_x_size),
                  row_loc = grid_loc(points, 2, grid_y_size),
                  grid_order = rep(NA, nrow(points)))

  points <- cbind(points,
                  # The fourth column for column and the fifth column for row
                  grid_index = grid_x_dim * (points[, "row_loc"] - 1) + points[, "col_loc"],
                  point_index = c(1:nrow(points)))

  for (i in 1:nrow(points)) {
    same_grid_index <- which(points[i, "grid_index"] == ordered_grid[ , "grid_index"])
    points[i, "grid_order"] <- ordered_grid[same_grid_index, "grid_order"]
  }



  # Calculate the distance between two points
  distance <- function(point1, point2) {
    return(sqrt(sum((point1 - point2)^2)))
  }

  points <- points[, -c(4:5)]

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

  # For each grid box with its neighboring, mark ordered and remaining points
  ordered_list <- vector("list", num_boxes)
  remaining_list <- vector("list", num_boxes)
  for (i in 1:num_boxes) {
    grid_order <- i
    sub_points_index <- which(points[ , "grid_order"] == i)
    remaining_list[[i]] <- c(remaining_list[[i]], sub_points_index)
    # Find points in the neighborhood
    grid_index <- points[sub_points_index[1], "grid_index"]
    neighboring_grid_index <- neighboring_grid(grid_index)

    for (k in 1:length(neighboring_grid_index)) {
      neighbor_points_index <- which(points[ , "grid_index"] == neighboring_grid_index[k])
      remaining_list[[i]] <- c(remaining_list[[i]], neighbor_points_index)
    }
  }

  # Order points
  ordered_points <- matrix(nrow = 0, ncol = 6)
  while (nrow(ordered_points) < nrow(points)) {
    for (i in 1:num_boxes){

      sub_points <- matrix(nrow = 0, ncol = 6)
      for (j in 1:length(remaining_list[[i]])) {
        sub_points <- rbind(sub_points, points[remaining_list[[i]][j], ])
      }

      # If there is not an ordered points in the neighborhood, find the point closest to center
      if (length(ordered_list[[i]]) == 0) {
        first_dist <- rep(NA, nrow(sub_points))
        for (j in 1:nrow(sub_points)) {
          first_dist[j] <- distance(sub_points[j, 1:2], ordered_grid[i, 1:2])
        }
        start_index <- which.min(first_dist)
        # Find the point's original index
        point_index <- sub_points[start_index, 6]
        # Find neighborhood with this point_index
        list_index <- which(sapply(remaining_list, function(x) point_index %in% x))
        # Mark points as ordered
        for (j in list_index) {
          ordered_list[[j]] <- c(ordered_list[[j]], point_index)
        }

        ordered_points <- rbind(ordered_points,
                                points[point_index, , drop = FALSE])

        remaining_list <- lapply(remaining_list, function(x) x[x != point_index])

      } else {
        # Current gird points
        current_grid_points <- matrix(nrow = 0, ncol = 6)
        current_grid_points <- rbind(current_grid_points,
                                     sub_points[sub_points[ , "grid_order"] == i, ])

        # Ordered points in the neighborhood
        neighbor_ordered_points <- matrix(nrow = 0, ncol = 6)
        for (j in 1:length(ordered_list[[i]])) {
          neighbor_ordered_points <- rbind(neighbor_ordered_points, points[ordered_list[[i]][j], ])
        }

        # Find the next ordered points
        for (j in 1:nrow(current_grid_points)) {
          num_neighbor_ordered_points <- nrow(neighbor_ordered_points)
          dist <- distance(current_grid_points[j, 1:2], neighbor_ordered_points[num_neighbor_ordered_points, 1:2])
          if (is.na(current_grid_points[j, "dist_vec"])) {
            current_grid_points[j, "dist_vec"] <- dist
            index <- current_grid_points[j, "point_index"]
            points[index, "dist_vec"] <- dist

          } else if (dist < current_grid_points[i, "dist_vec"]) {
            current_grid_points[j, "dist_vec"] <- dist
            index <- current_grid_points[j, "point_index"]
            points[index, "dist_vec"] <- dist
          }
        }
        current_grid_points <- build_heap(current_grid_points, compare_mmd)

        # Find the point's original index
        point_index <- current_grid_points[1, 6]
        # Find neighborhood with this point_index
        list_index <- which(sapply(remaining_list, function(x) point_index %in% x))
        # Mark points as ordered
        for (j in list_index) {
          ordered_list[[j]] <- c(ordered_list[[j]], point_index)
        }

        ordered_points <- rbind(ordered_points,
                                current_grid_points[1, ])

        remaining_list <- lapply(remaining_list, function(x) x[x != point_index])
      }
    }
  }
  return(ordered_points)
}


new_robot <- function(coordinates, direction) {
  valid_directions <- c("NORTH", "EAST", "SOUTH", "WEST")
  if (!(direction %in% valid_directions)) {
    stop("Invalid direction. Choose from NORTH, EAST, SOUTH, or WEST.")
  }
  
  robot <- list(
    coordinates = coordinates,
    direction = direction
  )
  
  class(robot) <- "robot"
  return(robot)
}

move <- function(a_robot, commands) {
  UseMethod("move")
}

move.robot <- function(a_robot, commands) {
  for (command in strsplit(commands, "")[[1]]) {
    if (command == "R") {
      a_robot <- turn_right(a_robot)
    } else if (command == "L") {
      a_robot <- turn_left(a_robot)
    } else if (command == "A") {
      a_robot <- advance(a_robot)
    } else {
      stop("Invalid command. Use 'R', 'L', or 'A'.")
    }
  }
  return(a_robot)
}

turn_right <- function(robot) {
  direction_order <- c("NORTH", "EAST", "SOUTH", "WEST")
  current_index <- which(direction_order == robot$direction)
  new_direction <- direction_order[(current_index %% 4) + 1]
  robot$direction <- new_direction
  return(robot)
}

turn_left <- function(robot) {
  direction_order <- c("NORTH", "WEST", "SOUTH", "EAST")
  current_index <- which(direction_order == robot$direction)
  new_direction <- direction_order[(current_index %% 4) + 1]
  robot$direction <- new_direction
  return(robot)
}

advance <- function(robot) {
  if (robot$direction == "NORTH") {
    robot$coordinates[2] <- robot$coordinates[2] + 1
  } else if (robot$direction == "SOUTH") {
    robot$coordinates[2] <- robot$coordinates[2] - 1
  } else if (robot$direction == "EAST") {
    robot$coordinates[1] <- robot$coordinates[1] + 1
  } else if (robot$direction == "WEST") {
    robot$coordinates[1] <- robot$coordinates[1] - 1
  }
  return(robot)
}

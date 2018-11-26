printMaze <- function(maze, rows, cols) {
  for (x in seq(1, rows)) {
    print(maze[((x-1)*cols +1) : (x*cols)])
  }
}

moveUp <- function(position, rows, cols) {
  newPosition <- position - cols
  if (newPosition < 1) {
    return (position)
  } else {
    return (newPosition)
  }
}

moveDown <- function(position, rows, cols) {
  newPosition <- position + cols
  if (newPosition > rows*cols) {
    return (position)
  } else { 
    return (position + cols)
  }
}

moveLeft <- function(position, rows, cols) {
  newPosition <- position - 1
  if ((position - 1) %/% cols != (newPosition - 1) %/% cols) {
    return (position)
  } else {
    return (position - 1)
  }
}

moveRight <- function(position, rows, cols) {
  newPosition <- position + 1
  if ((position - 1) %/% cols != (newPosition - 1) %/% cols) {
    return (position)
  } else { 
    return (position + 1)
  }
}

simulateSolution <- function(maze, solution, rows, cols) {
  # Update this function to serve as a fitness funcition
  # The simplest example is shown here: return 1 if the solution found the exit and 0 if it did not
  currentPosition <- grep('s', maze)
  for (move in solution) {
    oldPosition <- currentPosition
    if (move == 'U') {
      currentPosition <- moveUp(currentPosition, rows, cols)
    } else if (move == 'D') {
      currentPosition <- moveDown(currentPosition, rows, cols)       
    } else if (move == 'L') {
      currentPosition <- moveLeft(currentPosition, rows, cols)            
    } else if (move == 'R') {
      currentPosition <- moveRight(currentPosition, rows, cols)
    } else {
      print('Error: Incorrect solution format')
      return(-1)
    }
    if (maze[currentPosition] == '#') {
      currentPosition <- oldPosition
    }
    if (maze[currentPosition] == 'e') {
      return(1)
    }
  } 
  return(0)
}


geneticAlgorithm <- function(maze, rows, cols) {
  # Implement the genetic algorithm in this function
  # You should add additional parameters to the function as needed
}


maze1 <- c(' ', ' ', ' ', ' ', 'e',
           ' ', '#', '#', '#', '#',
           ' ', ' ', 's', ' ', ' ',
           '#', '#', '#', '#', ' ',
           ' ', ' ', ' ', ' ', ' ')
rows1 <- 5
cols1 <- 5
solution1 <- c('L', 'L','U', 'U', 'R', 'R', 'R', 'R', 'R') 

maze2 <- c('#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#',
           '#', '#', ' ', '#', ' ', '#', ' ', ' ', ' ', ' ', '#', ' ', ' ', '#', ' ', '#', '#',
           '#', '#', 'e', '#', '#', '#', '#', '#', '#', '#', '#', '#', ' ', ' ', ' ', '#', '#',
           '#', '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', '#', '#', ' ', ' ', ' ', '#', '#',
           '#', '#', ' ', ' ', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', '#', ' ', ' ', ' ',
           '#', '#', ' ', '#', ' ', '#', ' ', '#', ' ', ' ', ' ', ' ', '#', '#', ' ', '#', ' ',
           '#', ' ', ' ', '#', ' ', ' ', ' ', '#', '#', '#', '#', ' ', '#', '#', ' ', '#', ' ',
           '#', '#', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', '#', ' ', '#', ' ',
           '#', '#', ' ', '#', ' ', ' ', ' ', ' ', ' ', '#', '#', ' ', '#', '#', ' ', ' ', ' ',
           '#', ' ', ' ', ' ', '#', ' ', '#', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', '#', ' ',
           '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', ' ',
           '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', ' ',
           '#', ' ', '#', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', ' ', '#', ' ',
           '#', ' ', ' ', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', ' ', '#', ' ',
           '#', '#', ' ', '#', '#', '#', '#', ' ', '#', '#', ' ', ' ', ' ', ' ', ' ', '#', 's',
           '#', '#', ' ', ' ', '#', ' ', ' ', ' ', '#', '#', ' ', '#', '#', '#', ' ', '#', ' ',
           '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', ' ', '#', '#',
           '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#')

solution2 <- c('U', 'U', 'U', 'U', 'U', 'U', 'L', 'L', 'D', 'L', 'L', 'L', 'L', 'L', 'D', 'D', 'D', 'L', 'L', 'L', 'L', 'U', 'U', 'U', 'U', 'L', 'U', 'U', 'U', 'U', 'L', 'L', 'U', 'U')
cols2 <- 17
rows2 <- 18
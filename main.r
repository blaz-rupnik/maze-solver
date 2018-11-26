#declaring global constants
GENES <- c("U","D","L","R")
ELITISM_FACTOR <- 10
MATING_PERCENT <- 50
POPULATION_SIZE <- 100
NUM_OF_STEPS <- 8


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
  seeking <- grep('e', maze)
  seekingY <- (seeking %/% 5) + 1
  if (seeking %% 5 == 0){
    seekingY <- seeking %/% 5
  }
  seekingX <- seeking %% 5
  if (seekingX == 0){
    seekingX <- 5
  }
  multiplier = 1
  fitness <- 10
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
      fitness <- fitness + 10
    } else if (currentPosition == oldPosition){
      fitness <- fitness + 10
    }
    currentY <- (currentPosition %/% 5)+1
    if (currentPosition %% 5 == 0){
      currentY <- currentPosition %% 5
    }
    currentX <- currentPosition %% 5
    if (currentX == 0){
      currentX <- 5
    }
    euclidean <- ((currentX-seekingX)^2 + (currentY-seekingY)^2)*multiplier
    if (maze[currentPosition] == 'e') {
      return(0)
    }
    multiplier <- multiplier + 1
  }
  return(fitness)
}

trial = c("L","L","U","U","R","R","R","R")


geneticAlgorithm <- function(maze, rows, cols) {
  # Implement the genetic algorithm in this function
  # You should add additional parameters to the function as needed
  #Create initial population
  solved <- FALSE
  initial_population = createGenome()
  while (isFALSE(solved)){
    fitness_array <- c()
    for (i in 1:POPULATION_SIZE){
      fitness <- simulateSolution(maze1,initial_population[[i]],rows1,cols1)
      fitness_array <- c(fitness_array, fitness)
    }
    ordered_fitness = sort(fitness_array)
    indices = order(fitness_array)
    ordered_list <- vector("list", POPULATION_SIZE)
    for (j in 1:POPULATION_SIZE){
      ordered_list[[j]] <- initial_population[[indices[j]]]
    }
    print(ordered_list[[1]])
    if (ordered_fitness[1] == 0){
      solved <- TRUE
      break
    }
    
    new_population <- vector("list", POPULATION_SIZE)
    MATING_FACTOR <- 100 - ELITISM_FACTOR
    s <- 50
    for (st1 in 1:s){
      real_size <- (MATING_PERCENT*POPULATION_SIZE) %/% 100
      
      # initialise 2 random parents
      parent1 <- ordered_list[[sample(1:real_size, 1)]]
      parent2 <- ordered_list[[sample(1:real_size, 1)]]
      
      child <- mateParents(parent1,parent2)
      new_population[[st1]] <- child
    }
    for (st2 in s+1:(POPULATION_SIZE-s)){
      new_population[[st2]] <- ordered_list[[st2]]
    }
    initial_population <- new_population
  }
}

mateParents <- function(vector1,vector2){
  baby <- c()
  for (i in 1:NUM_OF_STEPS){
    probability <- runif(1, min = 0, max = 1)
    if (probability < 0.45){
      baby <- c(baby, vector1[i])
    } else if (probability < 0.9){
      baby <- c(baby, vector2[i])
    } else {
      baby <- c(baby, mutatedGene())
    }
  }
  return(baby)
}

createGenome <- function(){
  genome_length <- NUM_OF_STEPS
  l <- vector("list", POPULATION_SIZE)
  for (ii in 1:POPULATION_SIZE){
    one_genome <- c()
    for (i in 1:genome_length){
      gene <- mutatedGene()
      one_genome <- c(one_genome, gene)
    }
    l[[ii]] <- one_genome
  }
  return(l)
}

mutatedGene <- function(){
  random_number <- sample(1:4, 1)
  return(GENES[random_number])
}


maze1 <- c(' ', ' ', ' ', ' ', 'e',
           ' ', '#', '#', '#', '#',
           ' ', ' ', 's', ' ', ' ',
           '#', '#', '#', '#', ' ',
           ' ', ' ', ' ', ' ', ' ')

rows1 <- 5
cols1 <- 5
solution1 <- c('L', 'L','U', 'U', 'R', 'R', 'R', 'R', 'R')
testFitness <- c('L','L','L','L','L','L','L','L')

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
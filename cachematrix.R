#I wrote this based on the example given in the assignment 2 instructions
#This seems to work only on 2x2 matrix.

makeCacheMatrix <- function(x = matrix()) {
  c <- NULL
  setMatrix <- function(y) {
    x <<- y
    c <<- NULL
  } 
  getMatrix <- function() {
    x
  }
  cacheInverse <- function(z) {
    c <<- z
  }
  getInverse <- function() {
    c
  }
    list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
  
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }

  data <- x$getMatrix()
  
  inverse <- solve(data)
  
  x$cacheInverse(inverse)
  
  inverse
}

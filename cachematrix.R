#I wrote this based on the example given in the assignment 2 instructions

#This seems to work only on 2x2 matrix.

#makeCacheMatrix function will create a 2x2 matrix using x = matrix()

makeCacheMatrix <- function(x = matrix()) {
      c <- NULL #This is cache vector and is set to null at first 
  
  #setMatrix is to store matrix values
      setMatrix <- function(y) {
      x <<- y # y takes in new values of matrix as they are changed
      c <<- NULL #cache is again set to null as new values are set
    } 
  #getMatrix is for returning the matrix
      getMatrix <- function() {
      x
    }
  #cacheInverse is to store the cache 
      cacheInverse <- function(z) {
      c <<- z
  }
  #getInverse is to return the cache 
      getInverse <- function() {
      c
  }
      list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# The cacheSolve function will create the inverse of the matrix created by above function,makeCacheMatrix
cacheSolve <- function(x, ...) {
  #Get the cache value from getInverse() function from makeCacheMatrix function
      inverse <- x$getInverse()
      if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse) #return the inverse value if not null
  }
      data <- x$getMatrix() #get the matrix values and store to data vector
      inverse <- solve(data) #inverse using solve fuction
      x$cacheInverse(inverse)# store the inverse cache values
      inverse # return the final inversed values
}

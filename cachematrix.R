## The makeCacheMatrix is a function that creates and returns a list
## containing functions for setting and retrieving a matrix, as well as 
## setting and retrieving the inverse of the matrix. Furthermore, it will
## cache the inverse for efficiency.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve is a function that creates and returns a list
## containing functions for setting and retrieving a matrix, 
## and also for setting and retrieving the inverse of the matrix.
## Furthermore, it will cache the inverse for efficiency.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}

## makeCacheMatrix is a special function that caches the inverse of the matrix.
## cacheSolve takes the special matrix and returns the inverse which is already cached or calculates one.

## makeCacheMatrix takes the matrix as the input and returns a list of getter 
## and setter functions to operate on the special matrix.
## The function itself does not find the inverse but rather just stores the inverse if asked to.
##

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inv) {
    inverse <<- inv
  }
  
  getInverse <- function() inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## CacheSolve takes the special matrix as the input and returns the inverse of the 
## matrix in the cache if exists, else calculates one and sets in the cache.
## It supports only the square matrix.
##
## It uses the solve(x) function to calculate the inverse. The function will error 
## out if the inverse cannot be computed for a matrix.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}

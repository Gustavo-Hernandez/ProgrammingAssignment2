## Matrix inverse calculation and caching.
## This script aims to reduce the computational costs
## of recomputing the inverse of a matrix.


## makeCacheMatrix generates a list of functions to
## set and get a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Getters and Setters.
  inverse <- NULL
  set <- function(nm){
    x <<- nm
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  
  ##Return functions.
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve validates the existence of the inverse
## of a matrix, if nonexistent, it is calculated,
## assigned and returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # Search for cached inverse
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("Returning cached inverse ...")
    return(inverse)
  }
  
  #Calculate inverse
  mt <- x$get()
  localInverse <- solve(mt)
  
  #Assign Calculated Inverse
  x$setInverse(localInverse)
  
  localInverse
}

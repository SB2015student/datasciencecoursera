## There are 2 functions in this source code, the first will return a list of functions
## These functions will allow the setting and retrieving values of a matrix and its inverse (if calculated)
## The second will call the function to calculate inverse if its NULL and cache in memory

## The function starts with an empty matrix and defines the 'set' and 'get functions for the same
## The function also defines function to set and return the value of the matrix
## All the functions are returned to user as a list 

makeCacheMatrix <- function(x = matrix()) {
  # 'x=matrix()' -->initializes an empty matrix
  matInverse <- NULL
  #initializes a NULL for inverse
  
  # 'set' assigns the parameter passed to value of x
  set <- function(y) {
    x <<- y
    matInverse <<- NULL
  }
  
  # 'get'returns x
  get <- function() x
  
  #sets the inverse value
  setInverse <- function(solve) matInverse <<- solve
  
  #returns the value of the inverse
  getInverse <- function() matInverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
  
}


## This function will return cached value of the inverse matrix if it has been calculated
## if not calculated it will call the function to calculate value and update cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

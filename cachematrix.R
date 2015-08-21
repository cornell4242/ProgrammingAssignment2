## This function creates a special "vector", but in reality it is simply
## a LIST containing four functions.

## This function creates four functions that can, respectively:
##  1. set the value of the matrix
##  2. get the cached value of the matrix
##  3. set the value of the inverse matrix
##  4. get the cached value of the inverse matrix

## if setting a NEW original matrix, NULLS out the cached inverse to 
## force new calculation

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y){
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(inversematrix) m <<- inversematrix
    getinv <- function() m
    
    ## Put the functions in a neat package
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function takes the original matrix stored in cache, 
## if it exists - implies previously done - retrieves cached inverse
## if it is NULL (i.e. not previously created) - calculates and sets in 
## parent function

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv() # sets local m variable to cached inverse matrix, if exists
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  } else {
    data <- x$get() # gets data from cached matrix
    m <- solve(data,...)   # creates inverse matrix
    x$setinv(m) #sets new inverse matrix to cache
    m #returns the inverse matrix
  }
}
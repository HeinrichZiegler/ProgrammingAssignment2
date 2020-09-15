## This file provides two R functions to facilitate the caching of the 
## inverse matrix or a given matrix
## Assumption: the provided matrix is always valid!

## makeCacheMatrix is a function which creates an object containing the 
## matrix and adding the ability to remember the inverse matrix
## it will also allow reset the inverse matrix value, or change the matrix
## Whenever the matrix contained is changed, the inverse matrix will be 
## reset as well

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function "cacheSolve" will calculate the inverse matrix of "special"
## matrix provided with makeCacheMatrix, and store the inverse in the object
## provided
## However, if an inverse matrix already exists it will use the buffered value
## and return it without performing the solve() operation

cacheSolve <- function(x, ...) {
        
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  ## Return a matrix that is the inverse of 'x'
  i
}

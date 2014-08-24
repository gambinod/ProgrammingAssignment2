## These two functions work together to cache the inverse of a matrix.
## Inverting a matrix can be an expensive operation and these functions
## can save time by caching a copy rather than computing in repeatedly.

## This function is used to create a matrix object that contains
## a cached copy of the inverse of itself.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will return a cached copy of the inverse the matrix
## if it exists. Otherwise, it will calculate the inverse of the matrix,
## store it in cache and return it to caller.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

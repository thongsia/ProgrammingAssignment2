## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. The following are a pair of functions that cache the inverse
## of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cm <- NULL
  set <- function(y) {
    x <<- y
    cm <<- NULL
  }
  get <- function() x
  setcachematrix <- function(cachematrix) cm <<- cachematrix
  getcachematrix <- function() cm
  list(set = set, get = get,
       setcachematrix = setcachematrix,
       getcachematrix = getcachematrix)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cachesolve should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cm <- x$getcachematrix()
  if(!is.null(cm)) {
    message("getting cached data")
    return(cm)
  }
  data <- x$get()
  cm <- solve(data, ...)
  x$setcachematrix(cm)
  cm
}

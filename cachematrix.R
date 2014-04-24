## Matrix inversion can be a costly computation. We can make this more efficient by storing,
## or caching, the inverse. We compute the inverse on first call and on all subsequent calls
## we retrieve the stored inverse matrix.

## This function creates a "special" matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   set <- function(y) {
      x <<- y
      i <<- NULL
   }
   get <- function() x
   setinv <- function(inverse) i <<- inverse
   getinv <- function() i   
   list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special "matrix" object that
## can cache its inverse, cacheMatrix. It only computes the inverse if the inverse
## has not been already set.
cacheSolve <- function(x, ...) {
   i <- x$getinv()
   if(!is.null(i)) {
      message("getting cached data")
      return(i)
   }
   data <- x$get()
   i <- solve(data, ...)
   x$setinv(i)
   i
}

## The functions create a matrix, cache it's inverse and return the
## results.

## This function creates a matrix and caches it's inverse. it also
## contains a list of functions to set the matrix, get the matrix,
## get the inverted matrix and to set the inverted matrix. These
## functions are later used the cacheSolve-Function.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

## The Function cacheSolve computes the inverse matrix. If the inverse
## matrix has already been calculated, it is taken from the cache. the
## result gets returned

cacheSolve <- function(x, ...) {
            m <- x$getinv()
            if(!is.null(m)) {
                  message("getting cached data")
                  return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setinv(m)
            m
      }   

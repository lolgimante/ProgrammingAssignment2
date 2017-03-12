## A pair of functions that cache the inverse of a matrix.
##

## This function creates a special "matrix" object that can cache its inverse.

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
    setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinv(m)
  m
  ## Returns a matrix that is the inverse of 'x'
}

## examples:
## B = matrix(c(2, 4, 3, 1, 5, 7,13,2,4,2,1,1), nrow=3,   ncol=3)
## test<- makeCacheMatrix(B)
## cacheSolve(test)

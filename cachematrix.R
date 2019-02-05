## This assignment is to do functions that cache the inverse of a matrix
## Two function called makeCacheMatrix and cacheSolve

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
           x <<- y
           i <<- NULL
       }
       get <- function() x
       setsolve <- function(solve) i <<- solve
       getsolve <- function() i
       list(set = set, get = get,
            setsolve = setsolve,
            getsolve = getsolve) 
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getsolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
  i
}

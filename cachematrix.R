## Creates the inverse of a matrix and caches it for future use

## Creates objects and functions for cacheSolve to use

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) { 
    x <<- y
    s <<- NULL
  }
  get <- function() x 
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set=set,
       get=get,
       setsolve=setsolve,
       getsolve=getsolve)
}


## If inverse has already been calculated, retrieves it. Otherwise, inverses the
## matrix.

cacheSolve <- function(x, ...) {
  s <-x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
  ## Return a matrix that is the inverse of 'x'
}

## The functions below allow for the cashing of matrix inversion.
## This speeds up computation by preventing the need to repeatedly perfom inversion.

## makeCacheMatrix creates a matrix that is able to store its own inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv <<- solve(x)
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}





## cacheSolve either returns the stored inverse or it calculates the inverse.

cacheSolve  <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
  }
        ## Return a matrix that is the inverse of 'x'

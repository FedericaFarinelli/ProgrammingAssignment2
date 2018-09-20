## Federica Farinelli
## This function creates a special Matrix using the same structure 
## of the function makeVector provided for this assignement. It also uses
## <<- operator to assign a value to an object in an environment
## that is different from the current environment required for this assignement

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function provides the inverse of a matrix using the same structure of the
## function cachemean. The idea is to avoid costly calculation when the values 
## have been already calculated.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
  }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}

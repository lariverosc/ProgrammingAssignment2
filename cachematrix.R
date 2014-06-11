##Two functions that allow's the caching of the inverse for a matrix

## This functions represents a matrix along with its inverse
makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL
  set <- function(y) {
    x <<- y
    xInv <<- NULL
  }
  get <- function() x
  setInv <- function(inv) xInv <<- inv
  getInv <- function() xInv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function wraps solve to check if the inverse was already computed and cached,
## if not cached it compute and save the inverse, the x parameter must be a matrix
## wrapper using the makeCacheMatrix function i.e. makeCacheMatrix(x)
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}

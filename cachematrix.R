## Put comments here that give an overall description of what your
## functions do

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- matrix()
  set <- function(y) {
    x <<- y
    inv <<- matrix()
  }
  get <- function() x
  setinv <- function(invs) inv <<- invs
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  invs <- x$getinv()
  ##mat <- x$get()
  if(!is.na(invs[1,1])) {
    message("getting cached data")
    return(invs)
  }
  data <- x$get()
  invs <- solve(data, ...)
  x$setinv(invs)
  invs
}

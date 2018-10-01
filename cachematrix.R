## Put comments here that give an overall description of what your
## functions do

## This function creates the matrix object which can cache ist inverse.

makeCacheMatrix <- function(x=matrix()) {
  Matrix_inv <- NULL
  set <- function(y) {
    x <<- y
    Matrix_inv <<- NULL
  }
  get <- function()x
  set_inv <- function(inv) Matrix_inv <<- inv
  get_inv <- function()Matrix_inv
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}

## This function computes the inverse of the special "matrix" returned by the Matrix above. 
##In case the inverse has already been calculated, the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  Matrix_inv <- x$get_inv()
  if(!is.null(Matrix_inv)) {
    message("getting cached data")
    return(Matrix_inv)
  }
  data <- x$get()
  Matrix_inv <- solve(data, ...)
  x$set_inv(Matrix_inv)
  return(Matrix_inv)
}
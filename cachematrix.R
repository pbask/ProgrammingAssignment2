
## The following is a pair of functions that cache and compute the 
## inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mtx = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mtx <<- x
    inverse <<- NULL
  }
  get <- function() return(mtx)
  setinv <- function(inv) inverse <<- inv
  getinv <- function() return(inverse)
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the "matrix" returned by `makeCacheMatrix`. 
## If the inverse has already been calculated and the matrix is the same, 
## then `cacheSolve` will retrieve the inverse from the cache.

cacheSolve <- function(mtx, ...) {
  inv <- mtx$getinv()
  if(!is.null(inv)) {
    message("Getting cached data...")
    return(inv)
  }
  data <- mtx$get()
  inv <- solve(data, ...)
  mtx$setinv(inv)
  return(inv)
}
# Put comments here that give an overall description of what your
# functions do
# makeCacheMatrix creates a list containing a function to
# a. Set the value of matrix
# b. Get the value of matrix
# c. Set the value of inverse of matrix
# d. Get the value of inverse of matrix
makeCacheMatrix <- function(Matrix_1 = matrix()) {
  inverse <- NULL
  set <- function(m1) {
    Matrix_1 <<- m1;
    inverse <<- NULL;
  }
  get <- function() return(Matrix_1);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(Matrix_1, ...) {
  inverse <- Matrix_1$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- Matrix_1$get()
  invserse <- solve(data, ...)
  Matrix_1$setinv(inverse)
  return(inverse)
}    ## Return a matrix that is the inverse of 'x'


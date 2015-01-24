# Put comments here that give an overall description of what your
# functions do
# makeCacheMatrix creates a list containing a function to
# a. Set the value of matrix
# b. Get the value of matrix
# c. Set the value of inverse of matrix
# d. Get the value of inverse of matrix
makeCacheMatrix <- function(x = matrix()) 
{
  inverse <- NULL
  set <- function(x) 
  {
    x <<- x;
    inverse <<- NULL;
  }
  get <- function() return(x);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}
## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) 
{
  inverse <- x$getinv()
  if(!is.null(inverse)) 
    {
    message("Getting cached data...")
    return(inverse)
    }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinv(inverse)
  return(inverse)
}


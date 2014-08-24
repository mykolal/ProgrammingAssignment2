## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function returns a list of functions, which allow operations on matrix:
# $set(x) - (re)creates object;
# $get(x) - gets object;
# $setinv(inv) - caches object inversion;
# $getinv(inv) - returns inversion

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # Initialize inversion variable
  set <- function(y) { 
    x <<- y # (re)assign to the object
    inv <<- NULL # reset inversion variable
  }
  get <- function() x # return object
  setinv <- function(inversion) inv <<- inversion # save invertion
  getinv <- function() inv # return cached inversion
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) # returt list of methods
}


## Write a short comment describing this function
# This function returns cached inversion of matrix.
# If no cached inversion found (or matrix has been
# changed by set() method) it (re)calculates and stores
# inverted matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
  inv <- x$getinv() # get matrix
  if(!is.null(inv)) { # test if inversion has been calculated already
    # cached inversion found, returning it
    message("getting cached data")
    return(inv)
  }
  # no cached inversion found, (re)calculating
  data <- x$get() # geting matrix
  inv <- solve(data, ...) # calculating the inversion
  x$setinv(inv) # storing(caching) the inversion
  inv #returning inversion
}
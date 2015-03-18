## Constructs an R object for storing a matrix and its inverse
## 
## This function creates an R environment intended to store
## a matrix, along with its inverse matrix. It is meant to be
## used as a parameter to the cacheSolve() function.
##
## The environment initially stores the parameter 'x' for the
## matrix and the caller can retrieve it using the get() function
## and also retrieve its inverse matrix with getinv() as follows:
##
## cm <- makeCacheMatrix(x)
## cm$get() # returns x
## cm$getinv() # returns x's inverse
##
## The inverse is only calculated when getinv() is called for the
## first time, at which point it will store the inverse matrix for
## reuse. Subsequent calls will immediately return the cached result.
##
## It is noted that when working with large matrices, it may be
## desirable to clear the cached value. To this end a clearinv()
## function is provided that will discared the cached inverse matrix.
## This will release the memory used for it, at the cost of having
## getinv() recalculate it the next time it is called, which may be
## preferable when you know will not be reusing the inverse matrix
## for a while.
##
## Please also note that it is possible to reset this environment
## with another matrix value using the set() method. In this case
## the cached value of the inverse matrix (if any) is discarded in
## order for the getinv() method to recalculate it the next time it
## is called.
##
## Example usage:
## 
## cm <- makeCacheMatrix(x)
## cm$get() # returns x
## cm$getinv() # calculates and returns x's inverse
## cm$set(y) # discards x and x's inverse, stores y
## cm$get() # returns y
## cm$getinv() # calculates and returns y's inverse
## cm$getinv() # returns y's inverse immediately
## cm$clearinv() # discards the cached value for y's inverse
## cm$getinv() # calculates and returns y's inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getinv <- function() {
    if (is.null(inv)) {
      message("recalc")
      inv <<- solve(x)
    }
    inv
  }
  clearinv <- function() {
    inv <<- NULL
  }
  
  list(set = set, get = get, getinv=getinv, clearinv=clearinv)
}

## This function takes an encapsulated matrix created by
## the makeCacheMatrix function as a parameter, and returns
## its inverse matrix. The inverse matrix is cached so that
## subsequent calls to this function do not have to recalculate
## it.
##
## Example usage:
##
## m <- matrix(c(-1,-2,1,1), 2,2)
## minv <- cacheSolve(makeCacheMatrix(m))
## minv %*% m # identity matrix of size 2
cacheSolve <- function(x, ...) {
  x$getinv()
}

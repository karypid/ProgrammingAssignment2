## Constructs an R object for storing a matrix and its inverse
## 
## This function creates an R environment intended to store a
## a matrix, along with its inverse matrix. It is meant to be
## used as a parameter to the cacheSolve() function which will
## transparently remember the inverse matrix and then reuse it
## if it is ever needed again subsequently.
##
## The environment initially stores the parameter 'x' for the
## matrix and the caller can retrieve it using the get() method
## as follows:
##
## cm <- makeCacheMatrix(x)
## cm$get() # returns x
##
## The inverse matrix is initially not stored. Use the setinv()
## function to set the inverse matrix's value, which may then be
## retrieved with getinv(), as follows:
##
## cm <- makeCacheMatrix(x)
## cm$setinv(xinverse) # store xinverse in the environment
## cm$getinv() # returns xinverse
##
## It is noted that setinv does not check whether the value to
## be used is the actual inverse matrix, so the caller should not
## abuse it to store arbitrary values as it may break other code
## that relies it.
##
## Please note that it is possible to reset this environment with
## another matrix value using the set() method. In this case the
## cached value of the inverse matrix (if any) is discarded:
##
## cm <- makeCacheMatrix(x)
## cm$get() # returns x
## cm$setinv(xinverse) # store xinverse in the environment
## cm$getinv() # returns xinverse
## cm$set(y)
## cm$get() # returns y
## cm$getinv() # returns NULL
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(y) inv <<- y
  getinv <- function() inv
  
  list(set = set, get = get, setinv=setinv, getinv=getinv)
}

## This function takes an encapsulated matrix created by
## the makeCacheMatrix function as a parameter, and returns
## its inverse matrix. The inverse matrix is cached so that
## subsequent calls to this function do not have to recalculate
## it.
##
## Example usage:
##
## m<-matrix(c(2,3,2,2), 2,2)
## cacheSolve(makeCacheMatrix(m)) %*% m
cacheSolve <- function(x, ...) {
  inv = x$getinv()
  
  if (is.null(inv)) {
    d = x$get()
    inv <- solve(d)
    x$setinv(inv)
  }
  
  inv
}

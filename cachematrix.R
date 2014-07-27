## These functions are used to cache and retrieve a matrix inversion 

## Creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # inv carries the inverted matrix
  inv <- NULL
  make <- function (y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  makeInverse <- function(solve) inv <<- solve
  getInverse <- function() {inv}
  list(make = make, get = get,
       makeInverse = makeInverse,
       getInverse = getInverse)

}


## Computes the inverse of the matrix type created by makeCacheMatrix
## If the inverse has already been calculated then this function retrieves the
## answer rather than re-calculating it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  # check if the matrix has already been inversed, and if true return it
  if(!is.null(inv)) {
    # This message is used to give feedback that cache retrieval has occured
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$makeInverse(inv)
  inv
}

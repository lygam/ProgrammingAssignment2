## Returns the inverse of a given matrix
## Retrieves inverse from cache if it has already been calculated

## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inversemat <- NULL
  getmat <- function() x
  setinverse <- function(inverse) inversemat <<- inverse
  getinverse <- function() inversemat
  list(getmat=getmat, setinverse=setinverse, getinverse=getinverse)
}


## Computes the inverse of matrix returned by above function
## If the inverse has already been calculated, the cache is retrieved

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversemat <- x$getinverse()
  if(!is.null(inversemat)) {
    print("Inverse has already been calculated. Retrieving cached data...")
    return(inversemat)
  }
  initialmat <- x$getmat()
  inversemat <- solve(initialmat, ...)
  x$setinverse(inversemat)
  inversemat
}
## This is ProgrammingAssignment2 for the Coursera R Programming course.
## Author: Paul N.

## These functions create a matrix object with the ability to cache its inverse 
## and also computes the inverse if it does not exist already.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize everything and create getters and setters
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated it returns
## the cached inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    ## This gets the cached inverse if it exists.
    return(i)
  }
  ## If the cached inverse doesn't exist, it computes it gets the data and
  ## computes it here
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

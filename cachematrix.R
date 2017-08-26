## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## It returns a list of functions which act as a setter and getter
## for matrix and its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  inversem <- NULL
  set <- function(y) {
    x <<- y
    inversem <<- NULL
  }
  get <- function() x
  setinv <- function(invm) inversem <<- invm
  getinv <- function() inversem
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## It checks the availability of cached inverse matrix, 
## if available returns the cached value 
## else computes the inverse of a matrix
## stores the cached value and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invm <- x$getinv()
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data, ...)
  x$setinv(invm)
  invm
}

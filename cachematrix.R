## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.

## The first function, makeCacheMatrix creates and stores a special matrix and calcualtes it's inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv<- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Calculates the inverse of a matrix. If the inverse has already been calculated, it fetches the result from cache

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

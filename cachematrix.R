## Takes a matrix 'x' and creates an object from it that can be used with
## the 'cacheSolve' function.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(inv) s <<- inv
  getsolve <- function() s
  list (set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}

## Takes the object created by the 'makeCacheMatrix' function and returns
## a matrix inverse to it. It then caches the result within the object so if
## it is called on the same object again, it will return the cached matrix
## instead of performing the calculation again.
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("retrieving from cache")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}


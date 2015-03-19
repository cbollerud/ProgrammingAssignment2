## These are helper functions to cache the result of a
## matrix inverse (solve) function
## makeCacheMatrix is a wrapper for a matrix
## cacheSolve runs solve once and caches the results


## Returns a list of functions that are helpers to
## create a cached instance of the inverse of a matrix
## so it does not need to get re-calculated every time
## it is used
##   setMatrix      set the value of a matrix
##   getMatrix      get the value of a matrix
##   cacheInverse   get the cahced value (inverse of the matrix)
##   getInverse     get the cahced value (inverse of the matrix)
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
## Function will first check cache to see if inverse has been created
## if yes, it will return the cached version
## if no, it will create the inverse, add it to the cache, and return it
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}



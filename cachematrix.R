
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinvers <- function(invers) inv <<- invers
  getinvers <- function() inv
  list(set = set, get = get,
       setinvers = setinvers,
       getinvers = getinvers)
}



cacheSolve <- function(x, ...) {
  inv <- x$getinvers()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinvers(inv)
  inv
}

#Input a matrix that has to be cached in the environment
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
# Compute (if exist use previouse) inverse of matrix
cacheSolve <- function(x, ...) {
  inv <- x$getinvers()
  if(!is.null(inv)) {    #Warn if inversed matrix exist
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)  #inverse matrix
  x$setinvers(inv)  
  inv
}

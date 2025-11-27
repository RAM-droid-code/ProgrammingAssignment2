
## makeCacheMatrix: creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL         # reset cached inverse when matrix changes
  }
  
  get <- function() x
  
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  

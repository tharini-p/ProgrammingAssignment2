## The functions are used to cache the inverse of a matrix
## rather than compute it repeatedly
## Here are a pair of functions that cache 
## the inverse of a matrix.

## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_m <<- inverse
  getinverse <- function() inv_m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
inv_m <- x$getinverse()## Return a matrix that is the inverse of 'x'
if (!is.null(inv_m)) {
  message("getting cached data")
  return(inv_m)
}
mat <- x$get()
inv_m <- solve(mat, ...)
x$setinverse(inv_m)
inv_m
}

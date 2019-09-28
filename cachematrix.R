## Put comments here that give an overall description of what your
## functions do

## Function to cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  matrixinv <- NULL
  set <- function(n){
    x <<- n
    matrixinv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) matrixinv <<- inverse
  getInv <- function() matrixinv
  list(set = set, get = get, setInv = setInv,getInv = getInv)
}


## Function to Get inverse matrix

cacheSolve <- function(x, ...) {
       
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInv(inv)
  inv
}

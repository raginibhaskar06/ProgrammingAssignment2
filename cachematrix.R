## Put comments here that give an overall description of what your
## functions do

## Function to cache its inverse

makeCacheMatrix <- function(m = matrix()) {

  matrixinv <- NULL
  set <- function(n){
    m <<- n
    matrixinv <<- NULL
  }
  get <- function() m
  setInv <- function(inverse) matrixinv <<- inverse
  getInv <- function() matrixinv
  list(set = set, get = get, setInv = setInv,getInv = getInv)
}


## Function to Get inverse matrix

cacheSolve <- function(m, ...) {
       
  inv <- m$getInv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- m$get()
  inv <- solve(mat, ...)
  m$setInv(inv)
  inv
}

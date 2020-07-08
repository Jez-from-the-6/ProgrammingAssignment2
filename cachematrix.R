## A set of Functions to create a Matrix with cacheable Inverse.
##
## The functions make use of lexical scoping to bind inverse 
## attributes to Matrices


## expects: a Matrix "x", default: empty Matrix
## returns: a list, consisting of
## - setter and getter for the Matrix ("set", "get") 
## - setter and getter for the Inverse ("setInverse", "getInverse")
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL 
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(val) inverse <<- val
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## expects: a CacheMatrix-Object "x" created by "makeCacheMatrix"
## returns: the inverse of the Matrix in the CacheMatrix-Object
## description: it checks x for a cached Inverse value. 
##  if there is no cached value, it computes the inverse of the
##  matrix and caches the value in the CacheMatrix-Object for 
##  future calls
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}

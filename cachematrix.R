## This is a proposed solution to Assingment2
## where lexical scope is used to calculate and
## store in cache the inverse of a matrix

## Stores a matrix and its inverse
## Provides getter and setter methods:
## x is the matrix
## inv is the result of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## This function takes a matrix as an argument
## and returns its inverse, using makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

##usage: first create a matrix, call it mtx
##then call makeCacheMatrix(mtx) 
##with the created matrix as parameter (mtx)
##assing the result to a new variable: mc
##the call cacheSolve(mc)
##with mc the result from makeCacheMatrix
## call a second time cacheSolve(mc)
##you'll get the cached results :)
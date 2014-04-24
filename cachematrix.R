## These two functions can be used to cache the inverse of a matrix and then retrieve the cached value when needed, making the operation more efficient. 

## makeCacheMatrix is a function that receives a matrix and defines a set of functions to define and retrieve this matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  matrixInverse <- NULL
  
  setMatrix <- function(newMatrix) {
    x <<- newMatrix
    matrixInverse <<- NULL
  }
  
  getMatrix <- function() x
  
  setMatrixInverse <- function(newInverse) matrixInverse <<- newInverse
  
  getMatrixInverse <- function() matrixInverse
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}

##cacheSolve is a function that receives a matrix and returns its inverse, first trying to retrieve it from the cached value (or calculating it otherwise)

cacheSolve <- function(x, ...) {
  
  matrixInverse <- x$getMatrixInverse()
  
  if(!is.null(matrixInverse)) {
    message("getting cached data")
    return(matrixInverse)
  }
  
  data <- x$getMatrix()
  matrixInverse <- solve(data, ...)
  x$setMatrixInverse(matrixInverse)
  
  matrixInverse
  
}

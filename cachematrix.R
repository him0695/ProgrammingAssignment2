## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special matriz which is basically a list of functions to set and get
##  the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  setMatrix <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  getMatrix <- function(){
    x
  }
  
  setInverse <- function(invMat)
    inverseMatrix <<- invMat
  
  getInverse <- function()
    inverseMatrix
  
  list(
    get = getMatrix,
    set = setMatrix,
    getInverse = getInverse,
    setInverse = setInverse
  )
}


## Write a short comment describing this function
## This function returns the inverse of a special matrix created using makeCacheMatrix function. 
## Before returning it checks if the inverse is present in the cache, if yes then it returns it else it
##  calculates and returns it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)){
    return(inverseMatrix)
  }
  matrixData <- x$get()
  inverseMatrix <- solve(matrixData)
  x$setInverse(inverseMatrix)
  inverseMatrix
}

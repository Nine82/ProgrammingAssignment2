## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
## list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setmatrix <- function(y) {
    x <<- y ## <<- assigns value to object in an environment different from the current
    i <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(setmatrix=setmatrix, getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse)
}


## The following function returns the inverse of the matrix. 
## This function assumes that the matrix is always invertible.
##Return a matrix that is the inverse of 'x'
##If inverse has been calculated already return prior calculation
##if not previously calculated, do so now

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data.")
    return(i)
  }
  data <- x$getmatrix()
  i <- solve(data)
  x$setinverse(i)
  i
}
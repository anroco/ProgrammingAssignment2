## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  setMatrix <- function(matrix) {
    x <<- matrix
    invMatrix <<- NULL
  }
  getMatrix <- function() x
  setInvMatrix <- function(inv_matrix) invMatrix <<- inv_matrix
  getInvMatrix <- function() invMatrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$getInvMatrix()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  data <- x$getMatrix()
  inv_matrix <- solve(data, ...)
  x$setInvMatrix(inv_matrix)
  inv_matrix
}
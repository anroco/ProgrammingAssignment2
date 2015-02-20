## This function allows you to create a special matrix, returns a list of
## functions getMatrix and setMatrix, also returns the functions getInvMatrix
## and setInvMatrix which allow you to manage the inverted matrix.
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

## This function allows to get the inverse matrix of the matrix created with
## the function makeCacheMatrix. If the inverse matrix has not been calculated
## bove will be calculated and returned. If the inverse matrix has been 
## calculated above will be returned.
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
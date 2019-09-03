## Two functions that cache the inverse of a matrix
## File created for the 3rd week assignment of R-Programming course on Course
## 2nd course of John Hopkins Data Science Specialization


## Creates a matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initialize the inverse field
  m_i <- NULL
  
  ## Method that set the matrix
  setMatrix <- function( matrix ) {
    m <<- matrix
    m_i <<- NULL
  }
  
  ## Method that get the matrix
  getMatrix <- function() {
    ## Simply return the matrix
    m
  }
  
  ## Method that set the inverse of the matrix
  setInverseMatrix <- function(inverse) {
    m_i <<- inverse
  }
  
  ## Method that get the inverse of the matrix
  getInverseMatrix <- function() {
    ## Return the inverse matrix
    m_i
  }
  
  ## Return methods list
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## Compute the inverse of the matrix saved in the object created by "makeCacheMatrix"
## If the inverse has already been calculated, and matrix field was not changed,
## the inverse will be retrieved from the cache
cacheSolve <- function(x, ...) {
  
  ## Get the inverse matrix field value
  m <- x$getInverseMatrix()
  
  ## Return the inverse if its already set
  if( !is.null(m) ) {
    message("retrieving from cache")
    return(m)
  }
  
  ## Get the matrix
  data <- x$getMatrix()
  
  ## Compute the inverse
  m <- solve(data)
  
  ## Set the inverse matrix field value
  x$setInverseMatrix(m)
  
  ## Return the matrix
  m
}

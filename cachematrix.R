## This is an utility R file which helps in creating and caching of a special matrix  and 
## provides the function to inverse the special matrix and cache the inverse matrix.
## It uses below utility functions
## i. makeCacheMatrix
## ii.cacheSolve


## Function makeCacheMatrix- creates a special matrix for the input matrix and provides functions to access the special and input matrix
## Assumption : Input matrix should be valid invertible matrix
## @param x A Matrix 
## @return list List of functions(set,get,setInverseMatrix,getInverseMatrix)
## @examples
## makeCacheMatrix(invertibleMatrix)


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## Sets the input matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Returns the input matrix
  get <- function() x
  ## Sets the inverse matrix
  setInverseMatrix  <- function(inverseMatrix) m<<- inverseMatrix
  ## Returns the inverse matrix
  getInverseMatrix <- function() m
  list(set=set, get=get, setInverseMatrix=setInverseMatrix,getInverseMatrix=getInverseMatrix)
}


## This function cacheSolve returns the inverse of the supplied special matrix of type cacheMatrix
## First time it inverses the matrix using the "Solve" function and sets the same into cache.
## Subsequently returns the inverse from the cache 
## @param inputSplMatrix A special matrix of type makeCacheMatrix
## @return invX inverse of the input matrix
## @examples
## cacheSolve(cacheMatrix)

cacheSolve <- function(inputSplMatrix, ...) {
        ## Return a matrix that is the inverse of 'x'
  invX <- inputSplMatrix$getInverseMatrix()
  if(!is.null(invX)){
    message("Returning cache data")
    return(invX)
  }
  splMatrix <- inputSplMatrix$get()
  invX <- solve(splMatrix)
  inputSplMatrix$setInverseMatrix(invX)
  invX
}

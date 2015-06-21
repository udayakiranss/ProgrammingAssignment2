## This is a utility which helps in inversing the matrix of type cacheMatrix and caching the inversing matrix. 
## It has 2 functions
## i. makeCacheMatrix - Input: Matrix OutPut: List of functions() to get/set matrix along with methods to obtain the cached matrix
## ii.cacheSolve - Input: Spl Matrix of type cache matrix  Output : Inverse of spl matrix


## makeCacheMatrix caches the inverse matrix for the first time and subsequently same matrix is returned
##  It exposes 3 methods
## i.   get - It returns the supplied matrix
## ii.  set - Sets the supplied matrix
## ii.  setInverseMatrix - sets the InverseMatrix into the cache
## iii. getInverseMatrix - returns the Inverse matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverseMatrix  <- function(inverseMatrix) m<<- inverseMatrix
  getInverseMatrix <- function() m
  list(set=set, get=get, setInverseMatrix=setInverseMatrix,getInverseMatrix=getInverseMatrix)
}


## This function returns the inverse of the supplied special matrix of type cacehMatrix
## First time it inverses the matrix using the "Solve" function and sets the same into cache.
## Subsequently returns the inverse from the cache 
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

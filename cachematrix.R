## Assignment: Caching the Inverse of a Matrix

## In this assignment we write a pair of functions that cache the inverse 
## of a matrix.The caching is important to avoid reevaluating the matrix 
## inverses because the matrix inversion is a costly computational process. 


## The following "makeCacheMatrix" function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(inv) m <<- inv
  getinvmatrix <- function() m
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}


## The following "cacheSolve" function computes the inverse of the special 
## "matrix" returned by "makeCacheMatrix" above. If the inverse has already 
## been calculated (and the matrix has not changed), then "cacheSolve"  
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinvmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  origmatrix <- x$get()
  m <- solve(origmatrix,...)
  x$setinvmatrix(m)
  m
}

## An Example:

findInvMatrix = makeCacheMatrix(diag(c(2,4,5)))
cacheSolve(findInvMatrix)
cacheSolve(findInvMatrix) ## getting chached data 
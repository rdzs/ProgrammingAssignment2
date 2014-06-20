## The following functions are used to cache the inverse of a 
## square invertible matrix.
## 

## makeCacheMatrix: Creates a list containing a function to
## 1. set the matrix 'x'
## 2. get the matrix 'x'
## 3. set the inverse of the matrix 'x'
## 4. get the inverse of the matrix 'x'

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve: Solves the inverse of the matrix 'x' created 
## using makeCacheMatrix.
## It first checks if the inverse has already been solved. 
## If it has, it gets the inverse from the cache and skips
## the solve. If the inverse has not yet been solved, it 
## solves the inverse of the matrix and sets the inverse
## in the cache through the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

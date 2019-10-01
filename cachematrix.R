## Programming Assignment 2: Lexical Scoping

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # Check if there is a matrix
  if (!is.matrix(x)) {
    message("must be a two-dimensional data")
    return(NULL)
  }
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(solve) inv <<- solve
  
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix"
## returned by 'makeCacheMatrix' above.
## Returns NULL if the inverse cannot be calculated.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  
  # If we already have the inverse matrix calculated, then it returns
  if(!is.null(inv)) {
    
    message("getting cached data")
    return(inv)
    
  }
  
  data <- x$get()
  
  # Before, we need to know if it's possible
  # Check if x is square
  if(ncol(data) != nrow(data)) {
    
    message("matrix must be square")
    return(NULL)
    
  }
  
  # Ckeck if determinant is zero
  if(det(data) == 0) {
    
    message("matrix must be non-singular")
    return(NULL)
    
  }
  
  inv <- solve(data, ...)
  
  x$setinv(inv)
  
  inv
}

makeCacheMatrix <- function(x = matrix()) {
  # makeCacheVector creates a special "matrix" that is used with cacheSolve
  #
  # Input: 'x', an invertible matrix created in R
  #
  # Returns: a list containing a function to
  #   set the value of the matrix
  #   get the value of the matrix
  #   set the value of the inverse
  #   get the value of the inverse
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(new_inv) inv <<- new_inv
  getinv <- function() inv
  list(set = set, 
       get = get, 
       setinv = setinv, 
       getinv = getinv)
}


cacheSolve <- function(x, ..) {
  # cacheSolve returns a matrix that is the inverse of 'x'
  #
  # Input: 'x', an invertible matrix created by makeCacheMatrix
  #
  # Returns: the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  } 
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv 
}


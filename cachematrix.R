## These functions create an inverted matrix
## and solve the inversion of the matrix if it is not already cached

## Creates special matrix object as per instructions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  setinverse <- function() m <<- m
  getinverse <- function() {
    m <<- solve(x)
    m
  }
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## returns the matrix based on computed inverse matrix or cached matrix depending on cache

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m  <- solve(data, ...)
  m      
}

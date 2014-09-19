## These functions create an inverted matrix
## and solve the inversion of the matrix if it is not already cached

## Creates special matrix object 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  setinverse <- function(solve) m <<- solveinverse
  getinverse <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## returns the matrix based on computed inverse matrix or cached matrix depending on cache

cacheSolve <- function(x, ...) {
  m <- x$getmatrixA()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(x)
  m      
}

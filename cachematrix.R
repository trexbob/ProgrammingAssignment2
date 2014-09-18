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
  setmatrixA <- function(matrixA) m <<- matrixA
  getmatrixA <- function() m
  list(set = set, get = get,
       setmatrixA = setmatrixA,
       getmatrixA = getmatrixA)
}


## inverts the matrix based on whether it is cached or not

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

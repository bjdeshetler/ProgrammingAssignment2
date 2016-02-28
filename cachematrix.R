## This function is designed to cache the inverse of a matrix to reduce calculation time
## and retrieve the cache if the matrix has not changed

##Creates a list of functions to set and set the value of the vector and inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(inv_matrix) m <<- inv_matrix
  getmatrix <- function() m
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## Checks for and returns the stored inverse matrix of a matrix, else 
## calculates the inverse and stores it to the cache

cacheSolve <- function(x, ...) {
  m <- x$getmatix()
  if(!is.null(m)) {
    message('Getting cached data')
    return(m)
  }
  data <- x$get()
  m <- solve(x,...)
  x$setmatrix(m)
  m
}

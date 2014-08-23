## Two functions to 1) Cache a Matrix and 2) Calculate and Cache the Inverse of the Matrix.
## If Inverse is not cached, this function will cache for faster recall.

## makeCacheMatrix creates an initial matrix and allows a new matrix to be set.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## cacheSolve calculates the inverse of the matrix and caches it.  If called
## later, the function will return the cached inverse for faster use.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}

## These functions are used to calculate and to cache the inverse of a matrix.
## Computing the inverse of a matrix might be too complex and could cause 
## inefficiencies in our code. The below functions apply Lexical scoping to
## cache the matrix and its inverse to easily take these values when needed.

## makeCacheMatrix creates and caches a matrix. It consists of subfunctions which
## are mainly for data retrieval (eg. $get is used to display the matrix).

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function caches the inverse of the matrix. makeCacheMatrix is 
## the one that calculates the inverse of a matrix, but this is the one that
## calls that subfunction and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  my_matrix <- x$get()
  m <- solve(my_matrix, ...)
  x$setinv(m)
  m
}
